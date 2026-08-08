//! Function calls, record projections, and function loops.
//!
//! A Solve program has no call op: every call is lowered through the callee's
//! own checked result definition, with the caller's arguments bound on an
//! explicit stack so a parameter cannot escape the call that supplies it.

use super::*;

type RecordCondition<'dae> = (dae::ExprId<'dae>, solve::Reg, dae::ExprId<'dae>);

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    pub(super) fn function_fold_parameter(
        &self,
        fold: dae::FunctionFoldId<'dae>,
        carried: u32,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let values = self
            .function_fold_values
            .iter()
            .rev()
            .find_map(|(active, values)| (*active == fold).then_some(values))
            .ok_or_else(|| {
                LowerError::contract("function loop parameter escaped its checked fold", span)
            })?;
        values
            .get(carried as usize)
            .and_then(|value| value.get(scalar))
            .copied()
            .ok_or_else(|| {
                LowerError::contract("function loop parameter scalar is out of range", span)
            })
    }

    pub(super) fn record_field(
        &mut self,
        expression: dae::ExprId<'dae>,
        field: usize,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let node = self.node(expression);
        match node.operation() {
            dae::ExpressionOperation::Record(fields) => self.expression(
                fields
                    .get(field)
                    .ok_or_else(|| LowerError::contract("record field is out of range", span))?,
                scalar,
            ),
            dae::ExpressionOperation::Call {
                function,
                output,
                arguments,
            } => self.record_call_field(function, output, arguments, field, scalar, span),
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                self.record_field(definition.rhs(), field, scalar, span)
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) => self.function_parameter_record_field(parameter, field, scalar, span),
            dae::ExpressionOperation::Conditional(operands) => {
                self.record_conditional_field(operands, field, scalar, span)
            }
            dae::ExpressionOperation::Array(elements) => {
                let first = elements.get(0).expect("checked record array is nonempty");
                let element_count = self.record_field_scalar_count(first, field);
                let element = elements
                    .get(scalar / element_count)
                    .expect("checked record field scalar selects an array element");
                self.record_field(element, field, scalar % element_count, span)
            }
            dae::ExpressionOperation::Comprehension { domain, body } => {
                self.record_comprehension_field(domain, body, field, scalar, span)
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                self.indexed_record_field(expression, base, subscripts, field, scalar, span)
            }
            dae::ExpressionOperation::ArrayUpdate {
                base,
                value,
                subscripts,
            } => self.record_array_update_field(
                expression, base, value, subscripts, field, scalar, span,
            ),
            _ => Err(LowerError::contract(
                "record field has no checked aggregate definition",
                span,
            )),
        }
    }

    fn record_call_field(
        &mut self,
        function: dae::FunctionId<'dae>,
        output: u32,
        arguments: dae::ExpressionOperands<'dae>,
        field: usize,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        if self.function_arguments.len() >= 256 {
            return Err(LowerError::non_computable(
                "function lowering exceeded the checked recursion limit",
                span,
            ));
        }
        let result = self.function_result(function, output, span)?;
        let arguments: Vec<_> = arguments.iter().collect();
        let assertion = ActiveCallAssertion {
            function,
            arguments: arguments.clone(),
        };
        if self.call_action_compilation
            && self.active_call_assertions.contains(&assertion)
            && self.current_function_frame_matches(&assertion)
        {
            return self.record_field(result, field, scalar, span);
        }
        self.enter_context(ScalarContextFrame::Function {
            parent: self.context_id,
            function,
            arguments: arguments.clone(),
        });
        self.function_arguments.push(FunctionArgumentsFrame {
            function,
            arguments,
            activation_base: self.activation_path.len(),
        });
        let owns_assertion = self.active_call_assertions.insert(assertion.clone());
        let lowered = if owns_assertion {
            self.schedule_function_assertions(
                self.view
                    .function(function)
                    .expect("checked function identity resolves")
                    .statements(),
                span,
            )
        } else {
            Ok(())
        };
        let lowered = lowered.and_then(|()| self.record_field(result, field, scalar, span));
        if owns_assertion {
            self.active_call_assertions.remove(&assertion);
        }
        self.function_arguments.pop();
        self.leave_context();
        lowered
    }

    fn record_conditional_field(
        &mut self,
        operands: dae::ExpressionOperands<'dae>,
        field: usize,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let (conditions, fallback) = self.record_conditions(operands)?;
        self.push_inactive_conditions(&conditions);
        let mut selected = self.record_field(fallback, field, scalar, span)?;
        self.pop_activations(conditions.len());
        for (branch, condition) in conditions.iter().copied().enumerate().rev() {
            let value = self.record_conditional_branch(
                &conditions[..branch],
                condition,
                field,
                scalar,
                span,
            )?;
            selected = self.select(condition.1, value, selected, span)?;
        }
        Ok(selected)
    }

    fn record_conditions(
        &mut self,
        operands: dae::ExpressionOperands<'dae>,
    ) -> Result<(Vec<RecordCondition<'dae>>, dae::ExprId<'dae>), LowerError> {
        let fallback_index = operands.len() - 1;
        let mut conditions = Vec::with_capacity(fallback_index / 2);
        let mut fallback = operands
            .get(fallback_index)
            .expect("checked conditional has a fallback");
        for ordinal in (0..fallback_index).step_by(2) {
            let condition = operands
                .get(ordinal)
                .expect("checked conditional condition ordinal");
            let register = self.expression(condition, 0)?;
            if self.integer_register(register) == Some(0) {
                continue;
            }
            let value = operands
                .get(ordinal + 1)
                .expect("checked conditional value ordinal");
            if self.integer_register(register).is_some() {
                fallback = value;
                break;
            }
            conditions.push((condition, register, value));
        }
        Ok((conditions, fallback))
    }

    fn record_conditional_branch(
        &mut self,
        previous: &[RecordCondition<'dae>],
        condition: RecordCondition<'dae>,
        field: usize,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        self.push_inactive_conditions(previous);
        self.push_activation(condition.0, true);
        let value = self.record_field(condition.2, field, scalar, span);
        self.pop_activation();
        self.pop_activations(previous.len());
        value
    }

    fn push_inactive_conditions(&mut self, conditions: &[RecordCondition<'dae>]) {
        for (condition, _, _) in conditions {
            self.push_activation(*condition, false);
        }
    }

    fn pop_activations(&mut self, count: usize) {
        for _ in 0..count {
            self.pop_activation();
        }
    }

    fn function_parameter_record_field(
        &mut self,
        parameter: dae::FunctionParameterId<'dae>,
        field: usize,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let context = self.suspend_context();
        let Some(frame) = self.function_arguments.pop() else {
            self.resume_context(context);
            return Err(LowerError::contract(
                "record function parameter escaped its checked call",
                span,
            ));
        };
        let suspended_activations = self.activation_path.split_off(frame.activation_base);
        let argument = (frame.function == parameter.function())
            .then(|| frame.arguments.get(parameter.ordinal() as usize).copied())
            .flatten();
        let lowered = argument
            .ok_or_else(|| {
                LowerError::contract("record function parameter has no checked argument", span)
            })
            .and_then(|argument| self.record_field(argument, field, scalar, span));
        self.activation_path.extend(suspended_activations);
        self.function_arguments.push(frame);
        self.resume_context(context);
        lowered
    }

    fn record_comprehension_field(
        &mut self,
        domain: dae::DomainId<'dae>,
        body: dae::ExprId<'dae>,
        field: usize,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let body_count = self.record_field_scalar_count(body, field);
        let point = scalar / body_count;
        let values = self
            .view
            .domain(domain)
            .expect("checked comprehension domain resolves")
            .structured()
            .index_tuple_at(point)
            .expect("checked comprehension domain remains valid")
            .expect("checked record field scalar selects a domain point");
        self.enter_context(ScalarContextFrame::Domain {
            parent: self.context_id,
            domain,
            values: values.clone(),
        });
        self.domain_points.push((domain, values));
        let result = self.record_field(body, field, scalar % body_count, span);
        self.domain_points.pop();
        self.leave_context();
        result
    }

    fn indexed_record_field(
        &mut self,
        indexed: dae::ExprId<'dae>,
        base: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        field: usize,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let field_width = self.record_layout(indexed, field).field_width();
        let record_scalar = scalar / field_width;
        let field_scalar = scalar % field_width;
        let selected = ScalarSelector::from_points(self.view, &self.domain_points)
            .indexed_base_scalar(
                base,
                subscripts,
                self.node(indexed).value_type().dimensions(),
                record_scalar,
            )?;
        self.record_field(base, field, selected * field_width + field_scalar, span)
    }

    #[allow(clippy::too_many_arguments)]
    fn record_array_update_field(
        &mut self,
        updated: dae::ExprId<'dae>,
        base: dae::ExprId<'dae>,
        value: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        field: usize,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let field_width = self.record_layout(updated, field).field_width();
        let base_record = scalar / field_width;
        let field_scalar = scalar % field_width;
        let selected = ScalarSelector::from_points(self.view, &self.domain_points)
            .array_update_value_scalar(
                base,
                subscripts,
                self.node(value).value_type().dimensions(),
                base_record,
            );
        match selected {
            Ok(Some(value_record)) => self.record_field(
                value,
                field,
                value_record * field_width + field_scalar,
                span,
            ),
            Ok(None) => self.record_field(base, field, scalar, span),
            Err(LowerError::NonComputable { reason, .. })
                if reason == "array subscript is not compile-time computable" =>
            {
                self.dynamic_record_field_array_update(
                    base,
                    value,
                    subscripts,
                    field,
                    base_record,
                    field_scalar,
                    span,
                )
            }
            Err(error) => Err(error),
        }
    }

    fn record_field_scalar_count(&self, expression: dae::ExprId<'dae>, field: usize) -> usize {
        let layout = self.record_layout(expression, field);
        layout.outer_count() * layout.field_width()
    }

    fn record_layout(&self, expression: dae::ExprId<'dae>, field: usize) -> dae::RecordFieldLayout {
        self.view
            .record_field_layout(self.node(expression).value_type_id(), field)
            .expect("checked record projection has a finite field layout")
    }

    pub(super) fn function_fold_output(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        carried: u32,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let cache_key = self.function_fold_cache_key(fold, carried, scalar);
        if let Some(output) = self.function_fold_output_cache.get(&cache_key).copied() {
            return Ok(output);
        }
        let fold_view = self
            .view
            .function_fold(fold)
            .ok_or_else(|| LowerError::contract("function fold identity does not resolve", span))?;
        let mut values = fold_view
            .initial_values()
            .rhs_iter()
            .map(|initial| {
                (0..scalar_count(self.view, initial))
                    .map(|element| self.expression(initial, element))
                    .collect::<Result<Vec<_>, _>>()
            })
            .collect::<Result<Vec<_>, _>>()?;
        let domain = self
            .view
            .domain(fold_view.domain())
            .expect("checked function fold domain resolves");
        let structured = domain.structured();
        let point_count = structured.scalar_count().map_err(|error| {
            LowerError::contract(
                format!("checked function fold domain became invalid: {error}"),
                span,
            )
        })?;
        for point in 0..point_count {
            let indices = structured
                .index_tuple_at(point)
                .expect("checked function fold domain remains valid")
                .expect("checked function fold point is in range");
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
            let updates = fold_view
                .update_values()
                .rhs_iter()
                .map(|update| {
                    (0..scalar_count(self.view, update))
                        .map(|element| self.expression(update, element))
                        .collect::<Result<Vec<_>, _>>()
                })
                .collect::<Result<Vec<_>, _>>();
            let (_, previous) = self
                .function_fold_values
                .pop()
                .expect("function fold frame was just pushed");
            self.leave_context();
            self.domain_points.pop();
            self.leave_context();
            values = updates?;
            debug_assert_eq!(
                previous.len(),
                values.len(),
                "checked DAE fold preserves carried arity"
            );
        }
        let output = values
            .get(carried as usize)
            .and_then(|value| value.get(scalar))
            .copied()
            .ok_or_else(|| {
                LowerError::contract("function fold output scalar is out of range", span)
            })?;
        self.function_fold_output_cache.insert(cache_key, output);
        Ok(output)
    }

    pub(super) fn function_parameter(
        &mut self,
        parameter: dae::FunctionParameterId<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let context = self.suspend_context();
        let Some(frame) = self.function_arguments.pop() else {
            self.resume_context(context);
            return Err(LowerError::non_computable(
                "function parameter escaped its checked call owner",
                span,
            ));
        };
        let suspended_activations = self.activation_path.split_off(frame.activation_base);
        let argument = (frame.function == parameter.function())
            .then(|| frame.arguments.get(parameter.ordinal() as usize).copied())
            .flatten();
        let lowered = argument
            .ok_or_else(|| {
                LowerError::non_computable(
                    "function parameter escaped its checked call owner",
                    span,
                )
            })
            .and_then(|argument| self.expression(argument, scalar));
        self.activation_path.extend(suspended_activations);
        self.function_arguments.push(frame);
        self.resume_context(context);
        lowered
    }

    pub(super) fn function_call(
        &mut self,
        function: dae::FunctionId<'dae>,
        output: u32,
        arguments: dae::ExpressionOperands<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        if self.function_arguments.len() >= 256 {
            return Err(LowerError::non_computable(
                "function lowering exceeded the checked recursion limit",
                span,
            ));
        }
        let result = self.function_result(function, output, span)?;
        let arguments: Vec<_> = arguments.iter().collect();
        let assertion = ActiveCallAssertion {
            function,
            arguments: arguments.clone(),
        };
        if self.call_action_compilation
            && self.active_call_assertions.contains(&assertion)
            && self.current_function_frame_matches(&assertion)
        {
            return self.expression(result, scalar);
        }
        self.enter_context(ScalarContextFrame::Function {
            parent: self.context_id,
            function,
            arguments: arguments.clone(),
        });
        self.function_arguments.push(FunctionArgumentsFrame {
            function,
            arguments,
            activation_base: self.activation_path.len(),
        });
        let owns_assertion = self.active_call_assertions.insert(assertion.clone());
        let lowered = if owns_assertion {
            self.schedule_function_assertions(
                self.view
                    .function(function)
                    .expect("checked function identity resolves")
                    .statements(),
                span,
            )
        } else {
            Ok(())
        };
        let lowered = lowered.and_then(|()| self.expression(result, scalar));
        if owns_assertion {
            self.active_call_assertions.remove(&assertion);
        }
        self.function_arguments.pop();
        self.leave_context();
        lowered
    }

    /// Resolve the checked result definition one call lowers through.
    ///
    /// Solve executes only programs it owns. An MLS §12.9 external body is
    /// foreign code with no Solve op, so lowering fails with the call's exact
    /// provenance rather than emitting a substitute value.
    fn function_result(
        &self,
        function: dae::FunctionId<'dae>,
        output: u32,
        span: Span,
    ) -> Result<dae::ExprId<'dae>, LowerError> {
        let definition = self
            .view
            .function(function)
            .ok_or_else(|| LowerError::contract("function identity does not resolve", span))?;
        if let Some(external) = definition.external() {
            return Err(LowerError::non_computable(
                format!(
                    "external {} function `{}` calls `{}`, which the Solve runtime cannot execute",
                    external.language().as_str(),
                    definition.name(),
                    external.symbol()
                ),
                span,
            ));
        }
        definition
            .result_values()
            .rhs(output as usize)
            .ok_or_else(|| LowerError::contract("function result ordinal is out of range", span))
    }
}
