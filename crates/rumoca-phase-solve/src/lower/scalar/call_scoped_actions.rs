//! Specialize function assertions at one exact call context.

use super::*;
use crate::lower::call_scoped_actions::{
    CollectedCallAssertion, CollectedCallAssertionProgram, CollectedCallAssertionRoot,
};

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    pub(super) fn schedule_typed_pure_call_assertions(
        &self,
        call: dae::ExprId<'dae>,
        function: dae::FunctionId<'dae>,
        registered: &crate::lower::typed_functions::RegisteredCall<'dae>,
        call_span: Span,
    ) -> Result<(), LowerError> {
        if registered.assertions.is_empty() {
            return Ok(());
        }
        let action_program =
            self.typed_pure_call_assertion_program(call, function, registered, call_span, false)?;
        let action_program = std::sync::Arc::<[solve::LinearOp]>::from(action_program);
        let root_program = self
            .active_clock
            .is_none()
            .then(|| {
                self.typed_pure_call_assertion_program(call, function, registered, call_span, true)
            })
            .transpose()?
            .map(std::sync::Arc::<[solve::LinearOp]>::from);
        for (output_offset, assertion) in registered.assertions.iter().enumerate() {
            let message = self.literal_assertion_message(assertion.message, call_span)?;
            self.insert_assertion(
                root_program
                    .as_ref()
                    .map(|program| CollectedCallAssertionRoot::Shared {
                        owner: registered.owner,
                        program: std::sync::Arc::clone(program),
                        output_offset,
                    }),
                CollectedCallAssertionProgram::Shared {
                    owner: registered.owner,
                    program: std::sync::Arc::clone(&action_program),
                    output_offset,
                },
                message,
                assertion.provenance,
            )?;
        }
        Ok(())
    }

    fn typed_pure_call_assertion_program(
        &self,
        call: dae::ExprId<'dae>,
        function: dae::FunctionId<'dae>,
        registered: &crate::lower::typed_functions::RegisteredCall<'dae>,
        span: Span,
        root: bool,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let arguments = match self.node(call).operation() {
            dae::ExpressionOperation::Call { arguments, .. } => arguments,
            _ => {
                return Err(LowerError::contract(
                    "typed pure-call assertion owner is not a call expression",
                    span,
                ));
            }
        };
        let mut compiler = self.fork_for_call_action();
        compiler.call_action_compilation = true;
        let (start, replayed) = compiler
            .emit_typed_pure_call(call, function, arguments, span)?
            .ok_or_else(|| {
                LowerError::contract("typed pure-call assertion lost its issued invocation", span)
            })?;
        if replayed.owner != registered.owner || replayed.site != registered.site {
            return Err(LowerError::contract(
                "typed pure-call assertion changed its issued owner",
                span,
            ));
        }
        let activation = compiler.activation(span)?;
        for assertion in registered.assertions.iter() {
            let predicate_offset = replayed.site.outputs()[..assertion.predicate_output]
                .iter()
                .try_fold(0usize, |count, output| {
                    count.checked_add(output.value_type().scalar_count() as usize)
                })
                .ok_or_else(|| {
                    LowerError::contract("typed pure-call predicate offset overflows", span)
                })?;
            let predicate = u32::try_from(predicate_offset)
                .ok()
                .and_then(|offset| start.checked_add(offset))
                .ok_or_else(|| {
                    LowerError::contract("typed pure-call predicate register overflows", span)
                })?;
            let result = if root {
                let safe = compiler.constant(-1.0, span)?;
                let failed = compiler.constant(1.0, span)?;
                let active_indicator = compiler.select(predicate, safe, failed, span)?;
                compiler.select(activation, active_indicator, safe, span)?
            } else {
                let failed = compiler.unary(dae::UnaryOperator::Not, predicate, span)?;
                compiler.binary(dae::BinaryOperator::And, activation, failed, span)?
            };
            compiler
                .ops
                .push(solve::LinearOp::StoreOutput { src: result });
        }
        Ok(compiler.ops)
    }

    pub(in crate::lower) fn deferred_call_action_program<'recipe>(
        view: dae::DaeView<'dae>,
        layout: &'layout LoweredLayout<'dae>,
        recipes: impl IntoIterator<Item = &'recipe DeferredCallAssertion<'dae>>,
    ) -> Result<Vec<solve::LinearOp>, LowerError>
    where
        'dae: 'recipe,
    {
        let mut compiler = Self::new(view, layout, None);
        compiler.call_action_compilation = true;
        for recipe in recipes {
            compiler.domain_points = recipe.domain_points.clone();
            compiler.symbolic_domain_points = recipe.symbolic_domain_points.clone();
            compiler.function_arguments = recipe.function_arguments.clone();
            compiler.activation_path = recipe
                .activation_path
                .iter()
                .copied()
                .map(|guard| ActivationGuard {
                    register: None,
                    ..guard
                })
                .collect();
            compiler.active_clock = Some(recipe.active_clock);
            compiler.sampled_source = recipe.sampled_source;
            compiler.active_parameters = recipe.active_parameters.clone();
            compiler.active_call_assertions = recipe.active_call_assertions.clone();
            compiler.context_id = 0;
            compiler.context_stack.clear();
            let frame = recipe.function_arguments.last().ok_or_else(|| {
                LowerError::contract(
                    "deferred call assertion has no function context",
                    compiler.node(recipe.condition).provenance().span(),
                )
            })?;
            compiler.enter_context(ScalarContextFrame::Function {
                parent: 0,
                call: frame.call,
                function: frame.function,
                arguments: frame.arguments.clone(),
            });
            let span = compiler.node(recipe.condition).provenance().span();
            let safe = match recipe.fold {
                Some(fold) => {
                    compiler.function_fold_assertion_result(fold, recipe.condition, span)?
                }
                None => {
                    let activation = compiler.activation(span)?;
                    let condition = compiler.expression(recipe.condition, 0)?;
                    let inactive = compiler.unary(dae::UnaryOperator::Not, activation, span)?;
                    compiler.binary(dae::BinaryOperator::Or, inactive, condition, span)?
                }
            };
            let failed = compiler.unary(dae::UnaryOperator::Not, safe, span)?;
            compiler
                .ops
                .push(solve::LinearOp::StoreOutput { src: failed });
        }
        Ok(compiler.ops)
    }

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
        for statement in statements {
            self.schedule_fold_statement_assertion(fold, statement, call_span)?;
        }
        Ok(())
    }

    fn schedule_fold_statement_assertion(
        &self,
        fold: dae::FunctionFoldId<'dae>,
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
            } => self.collect_fold_assertion(fold, condition, message, provenance, call_span),
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
            .transpose()?
            .map(CollectedCallAssertionRoot::Ready);
        let action_program = self.call_assertion_action(condition, None, call_span)?;
        self.insert_assertion(root_program, action_program, message, provenance)
    }

    fn collect_fold_assertion(
        &self,
        fold: dae::FunctionFoldId<'dae>,
        condition: dae::ExprId<'dae>,
        message: dae::ExprId<'dae>,
        provenance: dae::DaeProvenance,
        call_span: Span,
    ) -> Result<(), LowerError> {
        let message = self.literal_assertion_message(message, call_span)?;
        let root_program = self
            .active_clock
            .is_none()
            .then(|| self.fold_assertion_root_program(fold, condition, call_span))
            .transpose()?
            .map(CollectedCallAssertionRoot::Ready);
        let action_program = self.call_assertion_action(condition, Some(fold), call_span)?;
        self.insert_assertion(root_program, action_program, message, provenance)
    }

    fn insert_assertion(
        &self,
        root_program: Option<CollectedCallAssertionRoot>,
        action_program: CollectedCallAssertionProgram<'dae>,
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
                    clock_owner: None,
                },
                clock_index: self.active_clock.map(|clock| clock.index() as usize),
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

    fn call_assertion_action(
        &self,
        condition: dae::ExprId<'dae>,
        fold: Option<dae::FunctionFoldId<'dae>>,
        span: Span,
    ) -> Result<CollectedCallAssertionProgram<'dae>, LowerError> {
        if self.active_clock.is_some()
            && self.derivative_definitions.is_none()
            && self.parameter_substitutions.is_none()
        {
            return Ok(CollectedCallAssertionProgram::Deferred(
                self.deferred_call_assertion(condition, fold),
            ));
        }
        let program = match fold {
            Some(fold) => self.fold_assertion_action_program(fold, condition, span)?,
            None => self.assertion_action_program(condition, span)?,
        };
        Ok(CollectedCallAssertionProgram::Ready(program))
    }

    fn deferred_call_assertion(
        &self,
        condition: dae::ExprId<'dae>,
        fold: Option<dae::FunctionFoldId<'dae>>,
    ) -> DeferredCallAssertion<'dae> {
        let symbolic_domain_points = self
            .symbolic_domain_points
            .iter()
            .filter(|(domain, _)| self.call_assertion_references_domain(condition, *domain))
            .cloned()
            .collect();
        DeferredCallAssertion {
            condition,
            fold,
            domain_points: self.domain_points.clone(),
            symbolic_domain_points,
            function_arguments: self.function_arguments.clone(),
            activation_path: self.activation_path.clone(),
            active_clock: self
                .active_clock
                .expect("deferred call assertions are clock-owned"),
            sampled_source: self.sampled_source,
            active_parameters: self.active_parameters.clone(),
            active_call_assertions: self.active_call_assertions.clone(),
        }
    }

    /// Whether one call-specialized assertion reads a compact-domain binder.
    ///
    /// Enclosing fold-update registers are allocation artifacts unless the
    /// assertion condition, an activation guard, or one of its bound call
    /// arguments can actually reach that domain. Capturing unrelated domains
    /// makes one semantic assertion acquire a new owner for every tensor
    /// projection and also leaks update-local registers into a later program.
    fn call_assertion_references_domain(
        &self,
        condition: dae::ExprId<'dae>,
        domain: dae::DomainId<'dae>,
    ) -> bool {
        std::iter::once(condition)
            .chain(
                self.function_arguments
                    .iter()
                    .flat_map(|frame| frame.arguments.iter().copied()),
            )
            .any(|root| self.expression_references_domain(root, domain))
            || self
                .activation_path
                .iter()
                .any(|guard| self.activation_condition_references_domain(guard.condition, domain))
    }

    fn activation_condition_references_domain(
        &self,
        owner: ActivationCondition<'dae>,
        domain: dae::DomainId<'dae>,
    ) -> bool {
        match owner {
            ActivationCondition::Expression(expression) => {
                self.expression_references_domain(expression, domain)
            }
            ActivationCondition::GuardedAssignment { trigger, guard, .. } => {
                let mut pending = vec![trigger, guard];
                let mut visited = HashSet::new();
                while let Some(condition) = pending.pop() {
                    if !visited.insert(condition) {
                        continue;
                    }
                    let operation = self
                        .view
                        .condition(condition)
                        .expect("checked activation condition resolves")
                        .operation();
                    match operation {
                        dae::ConditionOperation::Relation(relation) => {
                            let expression = self
                                .view
                                .relation(relation)
                                .expect("checked activation relation resolves")
                                .expression();
                            if self.expression_references_domain(expression, domain) {
                                return true;
                            }
                        }
                        dae::ConditionOperation::Discrete(expression) => {
                            if self.expression_references_domain(expression, domain) {
                                return true;
                            }
                        }
                        dae::ConditionOperation::Not(operand) => pending.push(operand),
                        dae::ConditionOperation::And(lhs, rhs)
                        | dae::ConditionOperation::Or(lhs, rhs)
                        | dae::ConditionOperation::AnyRise(lhs, rhs) => {
                            pending.push(lhs);
                            pending.push(rhs);
                        }
                        dae::ConditionOperation::Initial
                        | dae::ConditionOperation::Always
                        | dae::ConditionOperation::Clock(_) => {}
                    }
                }
                false
            }
        }
    }

    fn expression_references_domain(
        &self,
        root: dae::ExprId<'dae>,
        domain: dae::DomainId<'dae>,
    ) -> bool {
        let mut referenced = false;
        dae::for_each_expression(self.view, root, |_, expression| {
            referenced |= match expression.operation() {
                dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(binder)) => {
                    binder.domain() == domain
                }
                dae::ExpressionOperation::FunctionFoldParameter { fold, .. }
                | dae::ExpressionOperation::FunctionFoldOutput { fold, .. } => self
                    .view
                    .function_fold(fold)
                    .is_some_and(|fold| fold.domain() == domain),
                _ => false,
            };
        });
        referenced
    }

    fn fold_assertion_root_program(
        &self,
        fold: dae::FunctionFoldId<'dae>,
        condition: dae::ExprId<'dae>,
        span: Span,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let mut compiler = self.fork_for_call_action();
        let condition = compiler.function_fold_assertion_result(fold, condition, span)?;
        let safe = compiler.constant(-1.0, span)?;
        let failed = compiler.constant(1.0, span)?;
        let root = compiler.select(condition, safe, failed, span)?;
        compiler
            .ops
            .push(solve::LinearOp::StoreOutput { src: root });
        Ok(compiler.ops)
    }

    fn fold_assertion_action_program(
        &self,
        fold: dae::FunctionFoldId<'dae>,
        condition: dae::ExprId<'dae>,
        span: Span,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let mut compiler = self.fork_for_call_action();
        let condition = compiler.function_fold_assertion_result(fold, condition, span)?;
        let failed = compiler.unary(dae::UnaryOperator::Not, condition, span)?;
        compiler
            .ops
            .push(solve::LinearOp::StoreOutput { src: failed });
        Ok(compiler.ops)
    }

    pub(super) fn activation(&mut self, span: Span) -> Result<solve::Reg, LowerError> {
        self.materialize_activation_guards(span)?;
        let guards = self.activation_path.clone();
        self.activation_from_guards(&guards, true, span)
    }

    pub(super) fn fold_activation(&mut self, span: Span) -> Result<solve::Reg, LowerError> {
        self.materialize_activation_guards(span)?;
        let guards = self.activation_path[self.fold_guard_base..].to_vec();
        self.activation_from_guards(&guards, self.fold_guard_base == 0, span)
    }

    /// Materialize each branch condition under exactly its strict outer prefix.
    /// A deferred call-action compiler cannot reuse the original registers. A
    /// guard must not guard its own definition, but erasing the prefix would
    /// eagerly execute expensive or failing work from an inactive outer arm.
    fn materialize_activation_guards(&mut self, _span: Span) -> Result<(), LowerError> {
        if self
            .activation_path
            .iter()
            .all(|guard| guard.register.is_some())
        {
            return Ok(());
        }
        for index in 0..self.activation_path.len() {
            if self.activation_path[index].register.is_some() {
                continue;
            }
            let condition = self.activation_path[index].condition;
            let full_path = std::mem::take(&mut self.activation_path);
            let frame_activation_bases: Vec<_> = self
                .function_arguments
                .iter()
                .map(|frame| frame.activation_base)
                .collect();
            for frame in &mut self.function_arguments {
                // The deferred guard is compiled in its strict outer prefix.
                // A call frame opened inside a later guard cannot retain an
                // activation boundary beyond that temporarily visible prefix.
                frame.activation_base = frame.activation_base.min(index);
            }
            self.activation_path.extend_from_slice(&full_path[..index]);
            let result = self.materialize_activation_condition(condition, _span);
            self.activation_path = full_path;
            debug_assert_eq!(
                self.function_arguments.len(),
                frame_activation_bases.len(),
                "guard lowering preserves the checked function-frame stack"
            );
            for (frame, activation_base) in self
                .function_arguments
                .iter_mut()
                .zip(frame_activation_bases)
            {
                frame.activation_base = activation_base;
            }
            self.activation_path[index].register = Some(result?);
        }
        Ok(())
    }

    fn activation_from_guards(
        &mut self,
        guards: &[ActivationGuard<'dae>],
        include_clock: bool,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let mut activation = self.constant(1.0, span)?;
        if include_clock && let Some(clock) = self.active_clock {
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
        for guard in guards.iter().copied() {
            let mut value = match guard.register {
                Some(register) => register,
                None => self.materialize_activation_condition(guard.condition, span)?,
            };
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
        compiler.symbolic_domain_points = self.symbolic_domain_points.clone();
        compiler.function_arguments = self.function_arguments.clone();
        compiler.active_clock = self.active_clock;
        compiler.sampled_source = self.sampled_source;
        compiler.derivative_definitions = self.derivative_definitions;
        compiler.parameter_substitutions = self.parameter_substitutions;
        compiler.active_parameters = self.active_parameters.clone();
        compiler.activation_path = self
            .activation_path
            .iter()
            .copied()
            .map(|guard| ActivationGuard {
                register: None,
                ..guard
            })
            .collect();
        compiler.active_call_assertions = self.active_call_assertions.clone();
        compiler.call_action_compilation = true;
        compiler.suppress_function_assertions = self.suppress_function_assertions;
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
