//! One executed function frame retains its exact lexical conditional selection.

use super::*;

#[derive(Default)]
pub(super) struct NumericFunctionFrame<'dae> {
    pub(super) expressions: FxHashMap<u32, SourceValues>,
    pub(super) scope: Option<dae::FunctionScopeView<'dae>>,
    pub(super) function_entry: bool,
    pub(super) calls: FxHashMap<u32, Vec<SourceValues>>,
    pub(super) folds: FxHashMap<dae::FunctionFoldId<'dae>, Vec<SourceValues>>,
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
        let scope = self.scoped_expression_values.last()?.scope?;
        if scope.relation(definition.id()) != Some(dae::FunctionScopeRelation::Enclosing) {
            return None;
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
