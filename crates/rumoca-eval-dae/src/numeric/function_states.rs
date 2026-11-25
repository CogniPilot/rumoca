//! One executed function frame retains its exact lexical conditional selection.

use super::*;

#[derive(Default)]
pub(super) struct NumericFunctionFrame<'dae> {
    pub(super) expressions: FxHashMap<u32, SourceValues>,
    pub(super) scope: Option<dae::FunctionScopeView<'dae>>,
    pub(super) function_entry: bool,
    pub(super) calls: FxHashMap<u32, Vec<SourceValues>>,
    pub(super) folds: FxHashMap<dae::FunctionFoldId<'dae>, Vec<SourceValues>>,
    comprehension_owner: Option<dae::FunctionId<'dae>>,
    selected: FxHashMap<dae::FunctionConditionalId<'dae>, dae::FunctionBranchView<'dae>>,
}

impl<'dae> NumericFunctionFrame<'dae> {
    pub(super) fn lexical_frame(scope: Option<dae::FunctionScopeView<'dae>>) -> Self {
        Self {
            scope,
            ..Default::default()
        }
    }

    pub(super) fn function_entry() -> Self {
        Self {
            function_entry: true,
            ..Default::default()
        }
    }

    /// A comprehension owns point-local caches, not a new source statement
    /// scope. Its checked body identifies the invocation whose values it reads.
    pub(super) fn comprehension_frame(owner: Option<dae::FunctionId<'dae>>) -> Self {
        Self {
            comprehension_owner: owner,
            ..Default::default()
        }
    }
}

impl<'dae, F> NumericEvaluator<'dae, F>
where
    F: FnMut(dae::VariableView<'dae>, usize) -> Option<f64>,
{
    pub(super) fn enclosing_definition_frame(
        &self,
        definition: dae::FunctionDefinitionView<'dae>,
        contains: impl Fn(&NumericFunctionFrame<'dae>) -> bool,
    ) -> Option<&NumericFunctionFrame<'dae>> {
        let current = self.scoped_expression_values.last()?;
        if let Some(owner) = current.comprehension_owner {
            // Point frames have no source-statement region. Read only values
            // already executed in this function's live frames; the entry stop
            // below prevents a recursive call from reaching its caller.
            if definition.id().function() != owner {
                return None;
            }
        } else {
            let scope = current.scope?;
            if scope.relation(definition.id()) != Some(dae::FunctionScopeRelation::Enclosing) {
                return None;
            }
        }
        for frame in self.scoped_expression_values.iter().rev().skip(1) {
            if contains(frame) {
                return Some(frame);
            }
            // A function entry is a fresh invocation, even for recursion.
            if frame.function_entry {
                break;
            }
        }
        None
    }

    pub(super) fn function_assignment_group(
        &mut self,
        definitions: dae::FunctionDefinitionValues<'dae>,
    ) -> Result<(), NumericEvaluationError> {
        for definition in definitions.iter() {
            self.source_values(definition.rhs())?;
        }
        Ok(())
    }

    pub(super) fn ordered_function_conditional(
        &mut self,
        conditional: dae::FunctionConditionalBodyView<'dae>,
    ) -> Result<(), NumericEvaluationError> {
        let span = conditional.declaration().span();
        let mut selected = conditional.fallback();
        for (condition, branch) in conditional.conditions().zip(conditional.branches()) {
            match self.source_expression(condition)?.as_slice() {
                [Scalar::Boolean(false)] => {}
                [Scalar::Boolean(true)] => {
                    selected = branch;
                    break;
                }
                _ => {
                    return Err(failure(
                        NumericEvaluationErrorKind::InvalidValue,
                        "function conditional condition is not scalar Boolean",
                        span,
                    ));
                }
            }
        }
        self.function_statements(selected.statements())?;
        self.scoped_expression_values
            .last_mut()
            .expect("numeric source frame exists")
            .selected
            .insert(conditional.id(), selected);
        for definition in conditional.outputs() {
            let value = self
                .selected_function_value(conditional.id(), definition.target(), span)?
                .ok_or_else(|| function_result_error(span))?;
            self.cache_expression(definition.rhs(), value, false);
        }
        Ok(())
    }

    pub(super) fn require_function_assigned(
        &mut self,
        conditional: dae::FunctionConditionalBodyView<'dae>,
        value: dae::FunctionValueId<'dae>,
        definition: dae::FunctionDefinitionView<'dae>,
        span: Span,
    ) -> Result<(), NumericEvaluationError> {
        let Some(payload) = self.selected_function_value(conditional.id(), value, span)? else {
            let name = self
                .view
                .function(value.function())
                .and_then(|function| function.values().find(|candidate| candidate.id() == value))
                .map(|value| value.name())
                .ok_or_else(|| function_result_error(span))?;
            return Err(failure(
                NumericEvaluationErrorKind::AssertionFailed,
                format!("`{name}` is used without a value"),
                span,
            ));
        };
        self.cache_expression(definition.rhs(), payload, false);
        Ok(())
    }

    /// The carried form of the same action: the definition holds the value
    /// the executed path assigned, or its absence, and nothing is checked
    /// here. The read of the returned record's field is the checked use.
    pub(super) fn carry_function_assigned(
        &mut self,
        conditional: dae::FunctionConditionalBodyView<'dae>,
        value: dae::FunctionValueId<'dae>,
        definition: dae::FunctionDefinitionView<'dae>,
        span: Span,
    ) -> Result<(), NumericEvaluationError> {
        let payload = match self.selected_function_value(conditional.id(), value, span)? {
            Some(payload) => payload,
            None => {
                let count = self
                    .view
                    .expression(definition.rhs())
                    .and_then(|node| node.value_type().scalar_count())
                    .ok_or_else(|| function_result_error(span))?;
                NumericInitialValues::repeat(Scalar::Absent, count)
            }
        };
        self.cache_expression(definition.rhs(), payload, false);
        Ok(())
    }

    /// Whether `value` holds an absence this operation may carry. A carrier
    /// (a forward, a carried definition, the record and call result that hold
    /// one) passes it on; every other operation is a use, which fails as MLS
    /// §12.4.4 requires.
    pub(super) fn admit_absent(
        &self,
        value: &SourceValues,
        operation: dae::ExpressionOperation<'dae>,
        forwarded: bool,
        span: Span,
    ) -> Result<bool, NumericEvaluationError> {
        let absent = value.runs().any(|run| match run {
            NumericInitialRun::Repeat { value, .. } => matches!(value, Scalar::Absent),
            NumericInitialRun::Literal(values) => {
                values.iter().any(|value| matches!(value, Scalar::Absent))
            }
        });
        if !absent {
            return Ok(false);
        }
        if forwarded
            || matches!(
                operation,
                dae::ExpressionOperation::PassAssigned { .. }
                    | dae::ExpressionOperation::AssignedParameter { checked: false, .. }
                    | dae::ExpressionOperation::FunctionValue { .. }
                    // The carried action stores its state under this node.
                    | dae::ExpressionOperation::FunctionConditionalOutput { .. }
                    | dae::ExpressionOperation::Record(_)
                    | dae::ExpressionOperation::Call { .. }
            )
        {
            return Ok(true);
        }
        Err(failure(
            NumericEvaluationErrorKind::AssertionFailed,
            format!(
                "`{}` is used without a value",
                self.absent_source_name(operation)
            ),
            span,
        ))
    }

    /// The source name of the read that found no value: the field of the
    /// called function's result, or the parameter that arrived absent.
    fn absent_source_name(&self, operation: dae::ExpressionOperation<'dae>) -> String {
        match operation {
            dae::ExpressionOperation::Field { base, field } => {
                let base = self.view.expression(base);
                let output = base.and_then(|base| match base.operation() {
                    dae::ExpressionOperation::Call {
                        function, output, ..
                    } => self
                        .view
                        .function(function)
                        .and_then(|function| function.values().nth(output as usize))
                        .map(|value| value.name().to_string()),
                    _ => None,
                });
                let field = base
                    .and_then(|base| base.value_type().record_field_name(field as usize))
                    .map(ToString::to_string);
                match (output, field) {
                    (Some(output), Some(field)) => format!("{output}.{field}"),
                    _ => "a record field".to_string(),
                }
            }
            dae::ExpressionOperation::AssignedParameter { parameter, .. } => self
                .view
                .function(parameter.function())
                .and_then(|function| function.parameters().nth(parameter.ordinal() as usize))
                .map(|parameter| parameter.name().to_string())
                .unwrap_or_else(|| "a function input".to_string()),
            _ => "a value".to_string(),
        }
    }

    fn selected_function_value(
        &mut self,
        conditional: dae::FunctionConditionalId<'dae>,
        value: dae::FunctionValueId<'dae>,
        span: Span,
    ) -> Result<Option<SourceValues>, NumericEvaluationError> {
        let selected = self
            .scoped_expression_values
            .last()
            .and_then(|frame| frame.selected.get(&conditional))
            .copied()
            .ok_or_else(|| {
                failure(
                    NumericEvaluationErrorKind::UnsupportedOperation,
                    "function assigned state has no executed source statement",
                    span,
                )
            })?;
        match selected
            .outgoing(value)
            .ok_or_else(|| function_result_error(span))?
        {
            dae::FunctionAssignedState::Unassigned => Ok(None),
            dae::FunctionAssignedState::Assigned(id) => {
                let definition = self
                    .view
                    .function_definition(id)
                    .ok_or_else(|| function_result_error(span))?;
                self.source_values(definition.rhs()).map(Some)
            }
            dae::FunctionAssignedState::Conditional { owner, value } => {
                self.selected_function_value(owner, value, span)
            }
        }
    }
}
