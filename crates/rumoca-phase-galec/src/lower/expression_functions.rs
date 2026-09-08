//! Checked Modelica function calls, argument preparation, inlining, and
//! call-scoped assertions for the scalar GALEC expression boundary.

use super::*;

pub(super) struct EnteredFunctionCall<'dae, 'indices> {
    pub(super) call: dae::ExprId<'dae>,
    pub(super) function: dae::FunctionId<'dae>,
    pub(super) output: u32,
    pub(super) arguments: dae::ExpressionOperands<'dae>,
    pub(super) indices: &'indices [gast::Expression],
    pub(super) span: Span,
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

pub(super) struct TensorCallProjection {
    pub(super) contains_call: bool,
    pub(super) refusal_span: Option<Span>,
}

impl TensorCallProjection {
    fn record_call(&mut self, span: Span, region_boundary: bool) {
        self.contains_call = true;
        if region_boundary {
            self.refusal_span.get_or_insert(span);
        }
    }
}

fn append_projection_children<'dae>(
    operation: dae::ExpressionOperation<'dae>,
    region_boundary: bool,
    pending: &mut Vec<(dae::ExprId<'dae>, bool)>,
) {
    let mut append = |expression| pending.push((expression, region_boundary));
    match operation {
        dae::ExpressionOperation::Unary { operand, .. } => append(operand),
        dae::ExpressionOperation::Binary { lhs, rhs, .. } => {
            append(lhs);
            append(rhs);
        }
        dae::ExpressionOperation::Array(operands)
        | dae::ExpressionOperation::Record(operands)
        | dae::ExpressionOperation::Builtin {
            arguments: operands,
            ..
        } => operands.iter().for_each(&mut append),
        dae::ExpressionOperation::Field { base, .. } => append(base),
        dae::ExpressionOperation::Range(range) => {
            append(range.start().expression());
            if let Some(step) = range.explicit_step() {
                append(step.expression());
            }
            append(range.stop().expression());
        }
        dae::ExpressionOperation::Index { base, subscripts } => {
            append(base);
            subscript_expressions(subscripts).for_each(&mut append);
        }
        dae::ExpressionOperation::StringConversion { value, format, .. } => {
            append(value);
            match format {
                dae::StringConversionFormatView::Options {
                    minimum_length,
                    left_justified,
                    significant_digits,
                } => [minimum_length, left_justified, significant_digits]
                    .into_iter()
                    .flatten()
                    .for_each(&mut append),
                dae::StringConversionFormatView::Format { value } => append(value),
            }
        }
        dae::ExpressionOperation::ClockTransfer { source, .. } => append(source),
        dae::ExpressionOperation::Literal(_)
        | dae::ExpressionOperation::Coordinate(_)
        | dae::ExpressionOperation::Call { .. }
        | dae::ExpressionOperation::Conditional(_)
        | dae::ExpressionOperation::Comprehension { .. }
        | dae::ExpressionOperation::ArrayUpdate { .. }
        | dae::ExpressionOperation::FunctionValue { .. }
        | dae::ExpressionOperation::FunctionFoldParameter { .. }
        | dae::ExpressionOperation::FunctionFoldOutput { .. } => {}
    }
}

impl<'a, 'dae> ExpressionLowerer<'a, 'dae> {
    /// Whether this exact reaching definition is emitted to dominating local
    /// storage in the current function.
    ///
    /// `FunctionValue` identifies the checked reaching definition, so a
    /// same-function, non-elided definition is also the dominance witness used
    /// by `lower_function_value`. All other reads project the definition RHS.
    pub(super) fn uses_stored_function_value(
        &self,
        definition: dae::FunctionDefinitionView<'dae>,
    ) -> bool {
        self.function_scope == Some(definition.id().function())
            && !self
                .structural_function_locals
                .elides_definition(definition)
    }

    /// Follow only expression definitions this lowerer will actually project.
    ///
    /// A stored function local is a leaf even though the generic DAE arena walk
    /// can reach its historical right-hand side. A call is also a leaf: its
    /// arguments are evaluated once by the call materialization transaction.
    /// Those two stops keep call detection aligned with emitted execution
    /// instead of reconstructing it from every reachable DAE expression.
    pub(super) fn tensor_call_projection(
        &self,
        expression: dae::ExprId<'dae>,
    ) -> TensorCallProjection {
        let mut projection = TensorCallProjection {
            contains_call: false,
            refusal_span: None,
        };
        let mut pending = vec![(expression, false)];
        let mut seen = HashSet::new();
        while let Some((expression, region_boundary)) = pending.pop() {
            if !seen.insert((expression.index(), region_boundary)) {
                continue;
            }
            let node = self
                .view
                .expression(expression)
                .expect("checked tensor call projection expression resolves");
            match node.operation() {
                dae::ExpressionOperation::Call { .. } => {
                    projection.record_call(node.provenance().span(), region_boundary);
                }
                dae::ExpressionOperation::FunctionValue { definition, .. }
                    if self.uses_stored_function_value(definition) => {}
                dae::ExpressionOperation::FunctionValue { definition, .. }
                | dae::ExpressionOperation::FunctionFoldParameter { definition, .. }
                | dae::ExpressionOperation::FunctionFoldOutput { definition, .. } => {
                    pending.push((definition.rhs(), region_boundary));
                }
                dae::ExpressionOperation::Conditional(operands) => {
                    pending.extend(operands.iter().map(|operand| (operand, true)));
                }
                dae::ExpressionOperation::Comprehension { body, .. } => {
                    pending.push((body, true));
                }
                dae::ExpressionOperation::ArrayUpdate {
                    base,
                    value,
                    subscripts,
                } => {
                    pending.extend([(base, true), (value, true)]);
                    pending.extend(
                        subscript_expressions(subscripts).map(|subscript| (subscript, true)),
                    );
                }
                operation => append_projection_children(operation, region_boundary, &mut pending),
            }
        }
        projection
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
        // No representable ABI form. Substituting the body is the only emission
        // there is, so its failure is the model's refusal, not a decline.
        self.lower_entered_function_call(
            EnteredFunctionCall {
                call,
                function,
                output,
                arguments,
                indices,
                span,
            },
            |lowerer, result| lowerer.lower_at(result, indices),
        )
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

    pub(super) fn materialize_function_call(
        &mut self,
        call: dae::ExprId<'dae>,
        function: dae::FunctionId<'dae>,
        arguments: dae::ExpressionOperands<'dae>,
        span: Span,
    ) -> Result<Vec<gast::Name>, GalecTargetError> {
        self.record_reached_root_call(call);
        let function_view = self
            .view
            .function(function)
            .expect("checked function identity resolves");
        let key = self.materialized_function_call_key(call, function, arguments);
        let dominating = self.dominating_materialized_call(&key);
        let names = if let Some((dominating_key, names)) = dominating {
            self.record_reused_root_call(call, &dominating_key)?;
            if let Some(producer) = self
                .imported_materialized_call_regions
                .get(&dominating_key)
                .copied()
            {
                self.retained_call_dependencies.insert(producer);
            }
            names
        } else {
            let names = match self.disjoint_materialized_call_names(&key) {
                Some(names) => names,
                None => self.materialized_result_names(call, function_view, span)?,
            };
            let (arguments, argument_reads) =
                self.lower_direct_function_arguments(function_view, arguments, span)?;
            let function_call = gast::FunctionCall {
                function: user_functions::function_name(self.view, function_view)?,
                arguments,
            };
            self.commit_materialized_call_statement(
                call,
                key.clone(),
                &names,
                argument_reads,
                function_call,
                span,
            );
            self.materialized_function_calls.insert(key, names.clone());
            self.called_user_functions.insert(function.index());
            names
        };
        Ok(names)
    }

    fn materialized_function_call_key(
        &self,
        call: dae::ExprId<'dae>,
        function: dae::FunctionId<'dae>,
        arguments: dae::ExpressionOperands<'dae>,
    ) -> MaterializedFunctionCallKey {
        let owner = match self
            .view
            .expression(call)
            .expect("checked function call projection resolves")
            .operation()
        {
            dae::ExpressionOperation::Call { owner, .. } => owner.index(),
            _ => unreachable!("function-call key receives one checked call projection"),
        };
        MaterializedFunctionCallKey {
            call_path: self.materialized_call_path(),
            iteration_path: self.iteration_path(),
            activation_path: self.conditional_activation_path.clone(),
            owner,
            function: function.index(),
            arguments: arguments.iter().map(|argument| argument.index()).collect(),
        }
    }

    pub(super) fn materialized_call_path(&self) -> Vec<MaterializedCallKey> {
        self.call_frames
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
            .collect()
    }

    fn dominating_materialized_call(
        &self,
        key: &MaterializedFunctionCallKey,
    ) -> Option<(MaterializedFunctionCallKey, Vec<gast::Name>)> {
        self.materialized_function_calls
            .iter()
            .filter(|(candidate, _)| candidate.dominates(key))
            .max_by_key(|(candidate, _)| candidate.activation_path.len())
            .map(|(candidate, names)| (candidate.clone(), names.clone()))
    }

    fn disjoint_materialized_call_names(
        &self,
        key: &MaterializedFunctionCallKey,
    ) -> Option<Vec<gast::Name>> {
        self.materialized_function_calls
            .iter()
            .find(|(candidate, _)| {
                candidate.same_invocation(key)
                    && assigned_primitives::disjoint(
                        &candidate.activation_path,
                        &key.activation_path,
                    )
            })
            .map(|(_, names)| names.clone())
    }

    /// Emit one ABI call and its ownership action as one inseparable commit.
    fn commit_materialized_call_statement(
        &mut self,
        call: dae::ExprId<'dae>,
        key: MaterializedFunctionCallKey,
        names: &[gast::Name],
        argument_reads: HashSet<u32>,
        function_call: gast::FunctionCall,
        span: Span,
    ) {
        self.pending_prefix_statements.push(gast::Spanned::new(
            gast::Statement::MultiAssignment {
                targets: names.iter().cloned().map(gast::Reference::local).collect(),
                call: function_call,
            },
            span,
        ));
        self.record_committed_materialized_call(call, key, names.to_vec(), argument_reads);
    }

    fn record_committed_materialized_call(
        &mut self,
        call: dae::ExprId<'dae>,
        key: MaterializedFunctionCallKey,
        names: Vec<gast::Name>,
        argument_reads: HashSet<u32>,
    ) {
        let node = self
            .view
            .expression(call)
            .expect("checked call trace expression resolves");
        let owner = match node.operation() {
            dae::ExpressionOperation::Call { owner, .. } => owner,
            _ => unreachable!("call tracing receives one checked call projection"),
        };
        if let Some(scope) = self.function_scope {
            let action = FunctionCallAction {
                scope: scope.index(),
                call_path: self.materialized_call_path(),
                owner: owner.index(),
            };
            self.materialized_call_sources.insert(
                key,
                HashSet::from([CallExecutionSource::Function(action.clone())]),
            );
            self.evaluated_function_call_actions.push(action);
            return;
        }
        if node.function_scope().is_none() {
            let action = RootCallAction {
                owner: owner.index(),
                activation: self.conditional_activation_path.clone(),
                materialized: Some(MaterializedRootCall {
                    key: key.clone(),
                    names,
                    argument_reads,
                }),
            };
            self.materialized_call_sources.insert(
                key,
                HashSet::from([CallExecutionSource::Root(
                    ExpectedRootCallAction::from_emitted(&action),
                )]),
            );
            self.evaluated_root_call_actions.push(action);
        }
    }

    /// Commit exactly one successful call evaluation to the ownership stream
    /// appropriate for the lowering context.
    ///
    /// Model `DoStep` lowering records only model-scope roots: inner calls in
    /// a substituted body belong to that enclosing root transaction. Protected
    /// function lowering instead records the full enclosing call path so the
    /// same inner owner reached through distinct source invocations remains
    /// distinct while a repeated path is rejected.
    fn record_committed_call(&mut self, call: dae::ExprId<'dae>) {
        let node = self
            .view
            .expression(call)
            .expect("checked call trace expression resolves");
        let owner = match node.operation() {
            dae::ExpressionOperation::Call { owner, .. } => owner,
            _ => unreachable!("call tracing receives one checked call projection"),
        };
        if let Some(scope) = self.function_scope {
            self.evaluated_function_call_actions
                .push(FunctionCallAction {
                    scope: scope.index(),
                    call_path: self.materialized_call_path(),
                    owner: owner.index(),
                });
            return;
        }
        if node.function_scope().is_none() {
            self.evaluated_root_call_actions.push(RootCallAction {
                owner: owner.index(),
                activation: self.conditional_activation_path.clone(),
                materialized: None,
            });
        }
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
                names.extend(self.materialized_record_results(
                    result_type_id,
                    result_type,
                    call,
                    function,
                    output.name().as_str(),
                    span,
                )?);
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

    fn materialized_record_results(
        &mut self,
        result_type_id: dae::ValueTypeId<'dae>,
        result_type: &dae::ValueType,
        call: dae::ExprId<'dae>,
        function: dae::FunctionView<'dae>,
        output_name: &str,
        span: Span,
    ) -> Result<Vec<gast::Name>, GalecTargetError> {
        let mut names = Vec::new();
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
                &mut names,
            )?;
        }
        Ok(names)
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
    ) -> Result<(Vec<gast::Expression>, HashSet<u32>), GalecTargetError> {
        self.call_argument_read_captures.push(HashSet::new());
        let result = self.lower_direct_function_arguments_once(function, arguments, span);
        let reads = self
            .call_argument_read_captures
            .pop()
            .expect("direct argument read capture was entered");
        if let Some(parent) = self.call_argument_read_captures.last_mut() {
            parent.extend(reads.iter().copied());
        }
        result.map(|arguments| (arguments, reads))
    }

    fn lower_direct_function_arguments_once(
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
                let formal_scalar = scalar_type(
                    parameter_type.scalar_type(),
                    parameter.name().as_str(),
                    parameter.declaration().span(),
                )?;
                lowered.push(self.lower_function_argument(argument, formal_scalar, span)?);
                continue;
            }
            lowered.extend(self.lower_direct_record_argument(parameter, argument, span)?);
        }
        Ok(lowered)
    }

    fn lower_direct_record_argument(
        &mut self,
        parameter: dae::FunctionParameterView<'dae>,
        argument: dae::ExprId<'dae>,
        call_span: Span,
    ) -> Result<Vec<gast::Expression>, GalecTargetError> {
        let node = self
            .view
            .expression(argument)
            .expect("checked direct record actual resolves");
        if let dae::ExpressionOperation::Call {
            function,
            output,
            arguments,
            ..
        } = node.operation()
            && !user_functions::is_directly_lowerable(self.view, function)
        {
            return self.lower_entered_function_call(
                EnteredFunctionCall {
                    call: argument,
                    function,
                    output,
                    arguments,
                    indices: &[],
                    span: node.provenance().span(),
                },
                |lowerer, result| lowerer.lower_direct_record_fields(parameter, result, call_span),
            );
        }
        let projection = self.tensor_call_projection(argument);
        if let Some(span) = projection.refusal_span {
            return Err(unsupported(
                "record-argument-call-projection",
                "an effectful record argument beneath a conditional, update, or comprehension \
                 cannot yet be materialized as one exact call transaction"
                    .to_owned(),
                span,
            ));
        }
        self.lower_direct_record_fields(parameter, argument, call_span)
    }

    fn lower_direct_record_fields(
        &mut self,
        parameter: dae::FunctionParameterView<'dae>,
        argument: dae::ExprId<'dae>,
        call_span: Span,
    ) -> Result<Vec<gast::Expression>, GalecTargetError> {
        let parameter_type = self
            .view
            .value_type(parameter.value_type())
            .expect("checked direct record parameter type resolves");
        (0..parameter_type.record_field_count())
            .map(|field| {
                let (_, field_type) = self
                    .view
                    .record_field(parameter.value_type(), field)
                    .expect("checked direct record argument field resolves");
                self.lower_function_record_argument(argument, field, field_type, call_span)
            })
            .collect()
    }

    /// Lower one actual argument to the element type its formal parameter
    /// declares.
    ///
    /// MLS §10.6.13 makes Integer-to-Real conversion implicit wherever a Real
    /// is expected, and §6.7 applies that conversion element by element when
    /// the expected type is an array. The element target therefore comes from
    /// the formal, never from the actual: `identity(4)` is Integer[4, 4] and a
    /// Real[:, :] formal accepts it, while GALEC itself has no implicit
    /// conversion and needs the `real(...)` applied per element. `coerce` is
    /// the one place that decides which conversions exist, so scalars and
    /// aggregates both route through it and both refuse a Real actual reaching
    /// an Integer formal.
    fn lower_function_argument(
        &mut self,
        argument: dae::ExprId<'dae>,
        formal_scalar: gast::ScalarType,
        call_span: Span,
    ) -> Result<gast::Expression, GalecTargetError> {
        let node = self
            .view
            .expression(argument)
            .expect("checked function argument resolves");
        if node.value_type().dimensions().is_empty() {
            let value = self.lower(argument)?;
            return coerce(value, formal_scalar, node.provenance().span());
        }
        let actual_scalar = scalar_type(
            node.value_type().scalar_type(),
            "<function-argument>",
            node.provenance().span(),
        )?;
        // Storage may be handed over unchanged only when it already holds the
        // formal's element type. A converted aggregate is a new value, so it
        // needs its own bounded copy.
        if actual_scalar == formal_scalar
            && let Some(reference) = self.direct_whole_aggregate_reference(argument)?
        {
            return Ok(reference);
        }
        if expression_contains_array(self.view, argument) {
            let value = self.lower_aggregate_expression_as(argument, formal_scalar)?;
            return self.materialize_aggregate_function_argument(
                value,
                node.value_type().dimensions(),
                formal_scalar,
                call_span,
            );
        }
        self.materialize_tensor_function_argument(
            argument,
            None,
            node.value_type().dimensions(),
            formal_scalar,
            call_span,
        )
    }

    /// Preserve a whole aggregate whose checked DAE operation already denotes
    /// storage with the expression's exact type and shape.
    ///
    /// The DAE node supplies the aggregate type and shape once; this method only
    /// accepts operations whose checked identity denotes storage of that type.
    /// Computed aggregates return `None` and retain their explicit aggregate
    /// lowering. A function-local value also carries its exact reaching-
    /// definition proof, while a coordinate carries its checked variable
    /// identity. Keeping those references intact avoids a redundant bounded
    /// copy and a second full-size stack allocation in embedded C.
    pub(super) fn direct_whole_aggregate_reference(
        &mut self,
        argument: dae::ExprId<'dae>,
    ) -> Result<Option<gast::Expression>, GalecTargetError> {
        let node = self
            .view
            .expression(argument)
            .expect("checked function argument resolves");
        if node.value_type().dimensions().is_empty() {
            return Ok(None);
        }
        let span = node.provenance().span();

        if let dae::ExpressionOperation::Field { base, field } = node.operation() {
            let key = self.shared_record_field_key(base.index(), field as usize);
            if let Some(value) = self.materialized_shared_record_fields.get(&key) {
                return Ok(Some(value.clone()));
            }
            if let Some(value) = self.direct_record_function_argument(base, field as usize)? {
                return Ok(Some(value));
            }
        }

        if let dae::ExpressionOperation::FunctionValue { definition, .. } = node.operation() {
            if !self.uses_stored_function_value(definition) {
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

        self.direct_coordinate_aggregate_argument(coordinate, span)
    }

    fn direct_coordinate_aggregate_argument(
        &mut self,
        coordinate: dae::CoordinateView<'dae>,
        span: Span,
    ) -> Result<Option<gast::Expression>, GalecTargetError> {
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
        if !previous && let Some(reads) = self.call_argument_read_captures.last_mut() {
            reads.insert(variable.index());
        }
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
        let key = self.shared_record_field_key(argument.index(), field);
        if let Some(value) = self.materialized_shared_record_fields.get(&key) {
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
        &mut self,
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
        self.direct_whole_aggregate_reference(field)
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
        // The nest is about to be built with the guard inside it. Every element
        // would re-test a condition that mentions no iterator, so bind it once
        // here instead, under SPEC_0034 GAL-040 and the proofs
        // `guard_binding::permission` discharges, never unconditionally.
        for bound in guard_binding::bind_invariant_guards(
            &mut body,
            &iterators,
            dimensions,
            &self.temporary_namespace,
            &mut self.temporary_counter,
            call_span,
        ) {
            self.temporary_locals.push(bound.declaration);
            self.pending_prefix_statements.push(bound.binding);
        }
        let nest = user_functions::nest_tensor_loops(
            body,
            &iterators,
            &expression_projection::AxisBounds {
                extents: dimensions,
                proven: &|index, extent| self.prove_dynamic_index(index, extent, call_span).is_ok(),
            },
            call_span,
        );
        self.pending_prefix_statements.extend(nest);
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

    /// Enter, lower, and commit one substituted function invocation.
    ///
    /// The uncommitted entry token never escapes this method. The frame is
    /// removed on every result, and successful evaluation records its owner at
    /// the same boundary. Primitive and record projections therefore cannot
    /// forget the ownership action independently.
    pub(super) fn lower_entered_function_call<T>(
        &mut self,
        invocation: EnteredFunctionCall<'dae, '_>,
        lower: impl FnOnce(&mut Self, dae::ExprId<'dae>) -> Result<T, GalecTargetError>,
    ) -> Result<T, GalecTargetError> {
        self.record_reached_root_call(invocation.call);
        let result = self.enter_function_call(
            invocation.call,
            invocation.function,
            invocation.output,
            invocation.arguments,
            invocation.indices,
            invocation.span,
        )?;
        let lowered = lower(self, result);
        self.call_frames.pop();
        if lowered.is_ok() {
            self.record_committed_call(invocation.call);
        }
        lowered
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
        let function_view = self
            .view
            .function(function)
            .expect("checked function identity resolves");
        // Modelica actual arguments are evaluated before the callee body.  In
        // particular, an unused formal must not erase a nested call or its
        // assertion.  The prepared bindings are also the only values parameter
        // reads may project, so one actual cannot be evaluated twice.
        let prepared_arguments =
            self.prepare_inline_function_arguments(function_view, arguments, span)?;
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
        let owner = match self
            .view
            .expression(call)
            .expect("checked function call resolves")
            .operation()
        {
            dae::ExpressionOperation::Call { owner, .. } => owner,
            _ => unreachable!("function-call entry receives one checked call projection"),
        };
        self.call_frames.push(CallFrame {
            call,
            owner,
            function,
            arguments: arguments.iter().collect(),
            prepared_arguments,
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

    fn prepare_inline_function_arguments(
        &mut self,
        function: dae::FunctionView<'dae>,
        arguments: dae::ExpressionOperands<'dae>,
        call_span: Span,
    ) -> Result<Vec<PreparedInlineArgument>, GalecTargetError> {
        let mut prepared = Vec::with_capacity(arguments.len());
        for (parameter, argument) in function.parameters().zip(arguments.iter()) {
            let parameter_type = self
                .view
                .value_type(parameter.value_type())
                .expect("checked inline function parameter type resolves");
            if !parameter_type.is_record() {
                let scalar_type = scalar_type(
                    parameter_type.scalar_type(),
                    parameter.name().as_str(),
                    parameter.declaration().span(),
                )?;
                let value = self.lower_function_argument(argument, scalar_type, call_span)?;
                prepared.push(PreparedInlineArgument::Primitive(
                    self.bind_inline_argument(
                        value,
                        parameter_type.dimensions(),
                        scalar_type,
                        self.view
                            .expression(argument)
                            .expect("checked inline argument resolves")
                            .provenance()
                            .span(),
                    ),
                ));
                continue;
            }
            let fields = self.prepare_inline_record_argument(parameter, argument, call_span)?;
            prepared.push(PreparedInlineArgument::Record(fields));
        }
        Ok(prepared)
    }

    fn prepare_inline_record_argument(
        &mut self,
        parameter: dae::FunctionParameterView<'dae>,
        argument: dae::ExprId<'dae>,
        call_span: Span,
    ) -> Result<Vec<PreparedInlineValue>, GalecTargetError> {
        let node = self
            .view
            .expression(argument)
            .expect("checked inline record argument resolves");
        if let dae::ExpressionOperation::Call {
            function,
            output,
            arguments,
            ..
        } = node.operation()
            && !user_functions::is_directly_lowerable(self.view, function)
        {
            return self.lower_entered_function_call(
                EnteredFunctionCall {
                    call: argument,
                    function,
                    output,
                    arguments,
                    indices: &[],
                    span: node.provenance().span(),
                },
                |lowerer, result| {
                    lowerer.prepare_inline_record_fields(parameter, result, call_span)
                },
            );
        }
        let projection = self.tensor_call_projection(argument);
        if let Some(span) = projection.refusal_span {
            return Err(unsupported(
                "record-argument-call-projection",
                "an effectful record argument beneath a conditional, update, or comprehension \
                 cannot yet be materialized as one exact call transaction"
                    .to_owned(),
                span,
            ));
        }
        self.prepare_inline_record_fields(parameter, argument, call_span)
    }

    fn prepare_inline_record_fields(
        &mut self,
        parameter: dae::FunctionParameterView<'dae>,
        argument: dae::ExprId<'dae>,
        call_span: Span,
    ) -> Result<Vec<PreparedInlineValue>, GalecTargetError> {
        let parameter_type = self
            .view
            .value_type(parameter.value_type())
            .expect("checked inline record parameter type resolves");
        let argument_span = self
            .view
            .expression(argument)
            .expect("checked inline record argument resolves")
            .provenance()
            .span();
        let mut fields = Vec::with_capacity(parameter_type.record_field_count());
        for field in 0..parameter_type.record_field_count() {
            let (_, field_type_id) = self
                .view
                .record_field(parameter.value_type(), field)
                .expect("checked inline record parameter field resolves");
            let field_type = self
                .view
                .value_type(field_type_id)
                .expect("checked inline record field type resolves");
            let field_scalar = scalar_type(
                field_type.scalar_type(),
                parameter.name().as_str(),
                parameter.declaration().span(),
            )?;
            let value =
                self.lower_function_record_argument(argument, field, field_type_id, call_span)?;
            fields.push(self.bind_inline_argument(
                value,
                field_type.dimensions(),
                field_scalar,
                argument_span,
            ));
        }
        Ok(fields)
    }

    fn bind_inline_argument(
        &mut self,
        value: gast::Expression,
        dimensions: &[u32],
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> PreparedInlineValue {
        if matches!(
            &value,
            gast::Expression::Bool(_)
                | gast::Expression::Integer(_)
                | gast::Expression::Real(_)
                | gast::Expression::Ref(gast::Reference::Local(_))
        ) {
            return PreparedInlineValue {
                expression: value,
                dimensions: dimensions.to_vec(),
                scalar_type,
            };
        }
        let name = self.declare_function_argument(dimensions, scalar_type, span);
        self.pending_prefix_statements.push(gast::Spanned::new(
            gast::Statement::Assignment {
                target: gast::Reference::local(name.clone()),
                value,
            },
            span,
        ));
        PreparedInlineValue {
            expression: gast::Expression::Ref(gast::Reference::local(name)),
            dimensions: dimensions.to_vec(),
            scalar_type,
        }
    }

    pub(super) fn prepared_inline_argument(
        &self,
        parameter: dae::FunctionParameterId<'dae>,
    ) -> Result<PreparedInlineArgument, GalecTargetError> {
        self.call_frames
            .iter()
            .rev()
            .find(|frame| frame.function == parameter.function())
            .and_then(|frame| frame.prepared_arguments.get(parameter.ordinal() as usize))
            .cloned()
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: "function parameter used without its prepared call argument".to_owned(),
            })
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
                        owner: frame.owner.index(),
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
            iteration_path: self.iteration_path(),
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
        fold_id: dae::FunctionFoldId<'dae>,
        statements: dae::FunctionStatements<'dae>,
        span: Span,
    ) -> Result<(), GalecTargetError> {
        let fold = self
            .view
            .function_fold(fold_id)
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
        self.enter_iteration_point(
            IterationOwner::FunctionFold {
                function: fold_id.function().index(),
                fold: fold_id.ordinal(),
            },
            fold.domain().index(),
            frame_binders,
        )?;
        let assertion_start = self.pending_prefix_statements.len();
        let lowered = self.lower_function_assertions(statements);
        self.leave_iteration_point();
        self.loop_index_bounds.truncate(depth);
        lowered?;

        let mut body = self.pending_prefix_statements.split_off(assertion_start);
        for (binder, name) in binders.iter().zip(names).rev() {
            body = vec![gast::Spanned::new(
                gast::Statement::for_loop(gast::ForLoop::new(
                    Some(name),
                    gast::Expression::Integer(binder.lower),
                    (binder.step != 1).then_some(gast::Expression::Integer(binder.step)),
                    gast::Expression::Integer(binder.upper),
                    body,
                )),
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
}

fn expression_contains_array<'dae>(view: dae::DaeView<'dae>, root: dae::ExprId<'dae>) -> bool {
    let mut found = false;
    dae::for_each_expression(view, root, |_, expression| {
        found |= matches!(expression.operation(), dae::ExpressionOperation::Array(_));
    });
    found
}
