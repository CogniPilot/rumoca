mod function_states;
mod native_calls;
mod scalar;
mod tensor_values;
pub use scalar::Scalar;
/// Exact typed source runs; construction stays private to this evaluator.
pub type SourceValues = NumericInitialValues<Scalar>;
mod initial_values;
mod input_initialization;
pub use input_initialization::{
    InputFallbackBounds, InputInitialization, InputOrigin, InputOriginRun,
};

#[cfg(test)]
mod tests;

pub use initial_values::{NumericInitialRun, NumericInitialValues};

use rumoca_core::{Span, flatten_coordinates, row_major_coordinates};
use rumoca_ir_dae as dae;
use rustc_hash::FxHashMap;

use native_calls::native_function_call;
use tensor_values::{
    combine_extremum, diagonal_values, expand_flat_indices, expect_same_length,
    function_loop_point, identity_values, multiply_values, outer_product_values, range_values,
    skew_values, transpose_values,
};

pub(crate) use dae::InputInitializationPolicy;

/// The source expression selected before any host override or numerical view.
pub fn declaration_initial_expression<'dae>(
    variable: dae::VariableView<'dae>,
    input_policy: InputInitializationPolicy,
) -> Option<dae::ExprId<'dae>> {
    match variable.role() {
        dae::VariableRole::Parameter | dae::VariableRole::Constant => {
            variable.binding().or_else(|| variable.start())
        }
        dae::VariableRole::Input => variable.binding().or_else(|| {
            (input_policy != InputInitializationPolicy::RequireRuntimeDriver)
                .then(|| variable.start())
                .flatten()
        }),
        _ => variable.start().or_else(|| variable.binding()),
    }
}

/// Stable categories for failures while evaluating a checked DAE expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumericEvaluationErrorKind {
    CyclicDependency,
    MissingValue,
    NonStaticCoordinate,
    UnsupportedOperation,
    /// MLS §12.9 external body without a runtime implementation.
    ExternalFunction,
    ShapeMismatch,
    InvalidValue,
    OutOfBounds,
    Overflow,
    InvalidOverride,
    AssertionFailed,
}

/// A checked numeric-evaluation failure with exact expression provenance.
#[derive(Debug, Clone, thiserror::Error)]
#[error("{message}")]
pub struct NumericEvaluationError {
    kind: NumericEvaluationErrorKind,
    message: String,
    span: Span,
}

impl NumericEvaluationError {
    pub const fn kind(&self) -> NumericEvaluationErrorKind {
        self.kind
    }

    pub const fn span(&self) -> Span {
        self.span
    }
}

/// Evaluates compile-time numeric values from one branded checked DAE.
///
/// Parameter and constant coordinates are followed through their checked
/// bindings. By default, runtime inputs require a checked default binding or an
/// override for every scalar. Every other coordinate is rejected: this
/// evaluator never invents a runtime value. The optional override function is
/// applied at the variable boundary, so dependents observe the same overridden
/// values as the runtime layout.
pub struct NumericEvaluator<'dae, F = fn(dae::VariableView<'dae>, usize) -> Option<f64>> {
    view: dae::DaeView<'dae>,
    values: Vec<Option<SourceValues>>,
    expression_values: Vec<Option<SourceValues>>,
    scoped_expression_values: Vec<function_states::NumericFunctionFrame<'dae>>,
    evaluating: Vec<bool>,
    function_arguments: Vec<(dae::FunctionId<'dae>, Vec<SourceValues>)>,
    function_fold_values: Vec<(dae::FunctionFoldId<'dae>, Vec<SourceValues>)>,
    domain_points: Vec<(dae::DomainId<'dae>, Vec<i64>)>,
    override_value: F,
    input_policy: InputInitializationPolicy,
    input_initializations: Vec<Option<InputInitialization>>,
}

impl<'dae> NumericEvaluator<'dae> {
    pub fn new(view: dae::DaeView<'dae>) -> Self {
        Self::with_overrides(view, no_override)
    }
}

impl<'dae, F> NumericEvaluator<'dae, F>
where
    F: FnMut(dae::VariableView<'dae>, usize) -> Option<f64>,
{
    pub fn expression(
        &mut self,
        id: dae::ExprId<'dae>,
    ) -> Result<Vec<f64>, NumericEvaluationError> {
        Ok(self.expression_values(id)?.materialize())
    }
    pub fn expression_values(
        &mut self,
        id: dae::ExprId<'dae>,
    ) -> Result<NumericInitialValues, NumericEvaluationError> {
        Ok(self.source_values(id)?.map(Scalar::real))
    }
    /// A numerical Integer attribute refuses source bits outside its checked
    /// Binary64 projection profile instead of issuing a rounded bound.
    pub fn integer_attribute_values(
        &mut self,
        id: dae::ExprId<'dae>,
    ) -> Result<NumericInitialValues, NumericEvaluationError> {
        let values = self.source_values(id)?;
        values.numerical_attribute_values(dae::ScalarType::Integer, self.expression_span(id))
    }
    pub fn initial_value(
        &mut self,
        id: dae::VariableId<'dae>,
    ) -> Result<Vec<f64>, NumericEvaluationError> {
        Ok(self.initial_values(id)?.materialize())
    }
    pub fn initial_values(
        &mut self,
        id: dae::VariableId<'dae>,
    ) -> Result<NumericInitialValues, NumericEvaluationError> {
        Ok(self.source_initial_values(id)?.map(Scalar::real))
    }
    /// Evaluate through the same source execution owner, retaining exact payloads
    /// and source-issued Repeat/Literal structure after ordered fault checks.
    pub fn exact_expression_values(
        &mut self,
        id: dae::ExprId<'dae>,
    ) -> Result<SourceValues, NumericEvaluationError> {
        self.source_values(id)
    }
    pub fn with_overrides(view: dae::DaeView<'dae>, override_value: F) -> Self {
        Self::with_input_policy(
            view,
            override_value,
            InputInitializationPolicy::RequireRuntimeDriver,
        )
    }

    pub fn with_input_policy(
        view: dae::DaeView<'dae>,
        override_value: F,
        input_policy: InputInitializationPolicy,
    ) -> Self {
        Self {
            view,
            values: vec![None; view.variable_count()],
            expression_values: vec![None; view.expression_count()],
            scoped_expression_values: vec![function_states::NumericFunctionFrame::function_entry()],
            evaluating: vec![false; view.variable_count()],
            function_arguments: Vec::new(),
            function_fold_values: Vec::new(),
            domain_points: Vec::new(),
            override_value,
            input_policy,
            input_initializations: vec![None; view.variable_count()],
        }
    }

    /// Evaluate an expression whose only coordinates are static parameters.
    fn source_expression(
        &mut self,
        id: dae::ExprId<'dae>,
    ) -> Result<Vec<Scalar>, NumericEvaluationError> {
        Ok(self.source_values(id)?.materialize())
    }

    /// Evaluate static-parameter coordinates while retaining source-issued runs.
    fn source_values(
        &mut self,
        id: dae::ExprId<'dae>,
    ) -> Result<SourceValues, NumericEvaluationError> {
        let node = self
            .view
            .expression(id)
            .expect("finalized expression identity resolves");
        let operation = node.operation();
        let cacheable = !matches!(
            operation,
            dae::ExpressionOperation::Literal(_) | dae::ExpressionOperation::Coordinate(_)
        );
        let context_independent = node.variability() == dae::ExpressionVariability::Constant
            && node.binder_domain().is_none()
            && node.function_scope().is_none();
        if cacheable && let Some(value) = self.cached_expression(id, context_independent) {
            return Ok(value.clone());
        }
        let span = node.provenance().span();
        let mut value = self.evaluate_expression_operation(node, operation, span)?;
        require_finite(&value, span)?;
        if node.value_type().scalar_type() == dae::ScalarType::Real {
            value = value.map(|value| Scalar::Real(value.real()));
        }
        require_finite(&value, span)?;
        if cacheable {
            self.cache_expression(id, value.clone(), context_independent);
        }
        Ok(value)
    }

    fn evaluate_expression_operation(
        &mut self,
        node: dae::ExpressionView<'dae>,
        operation: dae::ExpressionOperation<'dae>,
        span: Span,
    ) -> Result<SourceValues, NumericEvaluationError> {
        let value = match operation {
            dae::ExpressionOperation::Literal(literal) => {
                return Ok(NumericInitialValues::repeat(
                    literal_value(literal, span)?,
                    1,
                ));
            }
            dae::ExpressionOperation::Coordinate(coordinate) => {
                return self.coordinate_value(coordinate, span);
            }
            dae::ExpressionOperation::Unary { operator, operand } => {
                return Ok(self.source_values(operand)?.map(|value| match operator {
                    dae::UnaryOperator::Plus => value,
                    dae::UnaryOperator::Negate => -value,
                    dae::UnaryOperator::Not => bool_value(value == 0.0),
                }));
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                self.binary_expression(operator, lhs, rhs, span)?
            }
            dae::ExpressionOperation::Conditional(operands) => {
                return self.conditional(operands, span);
            }
            dae::ExpressionOperation::Array(elements) => {
                let mut values = Vec::new();
                for element in elements.iter() {
                    values.extend(self.source_expression(element)?);
                }
                values
            }
            dae::ExpressionOperation::Record(fields) => return self.record(fields),
            dae::ExpressionOperation::Field { base, field } => {
                return self.record_field(base, field as usize, span);
            }
            dae::ExpressionOperation::Range(range) => range_values(
                range.start().value(),
                range.effective_step(),
                range.stop().value(),
                span,
            )?,
            dae::ExpressionOperation::Index { base, subscripts } => {
                self.index(base, subscripts, node.value_type(), span)?
            }
            dae::ExpressionOperation::ArrayUpdate {
                base,
                value,
                subscripts,
            } => self.array_update(base, value, subscripts, span)?,
            dae::ExpressionOperation::Builtin { builtin, arguments } => {
                return self.builtin(builtin, arguments, node.value_type().dimensions(), span);
            }
            dae::ExpressionOperation::Comprehension { domain, body } => {
                self.comprehension(domain, body, span)?
            }
            dae::ExpressionOperation::Call {
                owner,
                function,
                output,
                arguments,
            } => return self.function_call(owner, function, output, arguments, span),
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                return self.source_values(definition.rhs());
            }
            dae::ExpressionOperation::FunctionConditionalOutput { .. } => {
                return Err(failure(
                    NumericEvaluationErrorKind::UnsupportedOperation,
                    "function conditional projection has no executed source statement",
                    span,
                ));
            }
            dae::ExpressionOperation::FunctionFoldParameter { fold, carried, .. } => {
                return self.function_fold_parameter(fold, carried, span);
            }
            dae::ExpressionOperation::FunctionFoldOutput {
                fold,
                carried,
                definition,
            } => {
                return self.function_fold(fold, carried, definition, span);
            }
            dae::ExpressionOperation::StringConversion { .. } => {
                return Err(failure(
                    NumericEvaluationErrorKind::UnsupportedOperation,
                    "String conversion is outside the numeric DAE evaluator",
                    span,
                ));
            }
            dae::ExpressionOperation::ClockTransfer { source, .. } => {
                return self.source_values(source);
            }
        };
        Ok(NumericInitialValues::literal(value))
    }

    fn function_fold_parameter(
        &self,
        fold: dae::FunctionFoldId<'dae>,
        carried: u32,
        span: Span,
    ) -> Result<SourceValues, NumericEvaluationError> {
        self.function_fold_values
            .iter()
            .rev()
            .find(|(active, _)| *active == fold)
            .and_then(|(_, values)| values.get(carried as usize))
            .cloned()
            .ok_or_else(|| {
                failure(
                    NumericEvaluationErrorKind::UnsupportedOperation,
                    "function loop transition parameter escaped its fold",
                    span,
                )
            })
    }

    fn cached_expression(
        &self,
        id: dae::ExprId<'dae>,
        context_independent: bool,
    ) -> Option<&SourceValues> {
        if context_independent {
            return self.expression_values[id.index() as usize].as_ref();
        }
        let current = self.scoped_expression_values.last()?;
        if let Some(value) = current.expressions.get(&id.index()) {
            return Some(value);
        }
        let node = self.view.expression(id)?;
        let definition = match node.operation() {
            dae::ExpressionOperation::FunctionValue { definition, .. }
            | dae::ExpressionOperation::FunctionConditionalOutput { definition, .. }
            | dae::ExpressionOperation::FunctionFoldOutput { definition, .. } => definition,
            _ => return None,
        };
        self.enclosing_definition_frame(definition, |frame| {
            frame.expressions.contains_key(&id.index())
        })?
        .expressions
        .get(&id.index())
    }

    fn cache_expression(
        &mut self,
        id: dae::ExprId<'dae>,
        value: SourceValues,
        context_independent: bool,
    ) {
        if !context_independent && let Some(values) = self.scoped_expression_values.last_mut() {
            values.expressions.insert(id.index(), value);
        } else {
            self.expression_values[id.index() as usize] = Some(value);
        }
    }

    fn record(
        &mut self,
        fields: dae::ExpressionOperands<'dae>,
    ) -> Result<SourceValues, NumericEvaluationError> {
        let mut values = SourceValues::default();
        for field in fields.iter() {
            values.append(self.source_values(field)?);
        }
        Ok(values)
    }

    fn record_field(
        &mut self,
        base: dae::ExprId<'dae>,
        field: usize,
        span: Span,
    ) -> Result<SourceValues, NumericEvaluationError> {
        let base_node = self
            .view
            .expression(base)
            .expect("checked record base resolves");
        let layout = self
            .view
            .record_field_layout(base_node.value_type_id(), field)
            .ok_or_else(|| {
                failure(
                    NumericEvaluationErrorKind::UnsupportedOperation,
                    "record field layout is not finite",
                    span,
                )
            })?;
        let value = self.source_values(base)?;
        let expected = layout
            .outer_count()
            .checked_mul(layout.record_width())
            .ok_or_else(|| invalid_record_layout(span))?;
        if value.len() != expected {
            return Err(invalid_record_layout(span));
        }
        if layout.record_width() == 0 {
            return Ok(SourceValues::default());
        }
        let mut projected = SourceValues::default();
        for record in 0..layout.outer_count() {
            let start = record * layout.record_width() + layout.field_offset();
            projected.append(
                value
                    .slice(start, layout.field_width())
                    .ok_or_else(|| invalid_record_layout(span))?,
            );
        }
        Ok(projected)
    }

    fn coordinate_value(
        &mut self,
        coordinate: dae::CoordinateView<'dae>,
        span: Span,
    ) -> Result<SourceValues, NumericEvaluationError> {
        if let dae::CoordinateView::FunctionParameter(parameter) = coordinate {
            let arguments = self
                .function_arguments
                .iter()
                .rev()
                .find(|(function, _)| *function == parameter.function())
                .map(|(_, arguments)| arguments)
                .ok_or_else(|| function_parameter_error(span))?;
            return arguments
                .get(parameter.ordinal() as usize)
                .cloned()
                .ok_or_else(|| function_ordinal_error(span));
        }
        if let dae::CoordinateView::Binder(binder) = coordinate {
            return self
                .domain_points
                .iter()
                .rev()
                .find(|(domain, _)| *domain == binder.domain())
                .and_then(|(_, point)| point.get(binder.ordinal() as usize))
                .map(|value| NumericInitialValues::repeat(Scalar::Integer(*value), 1))
                .ok_or_else(|| {
                    failure(
                        NumericEvaluationErrorKind::UnsupportedOperation,
                        "domain binder escaped its checked owner",
                        span,
                    )
                });
        }
        if let dae::CoordinateView::ClockInterval(clock) = coordinate {
            return Ok(NumericInitialValues::repeat(
                Scalar::Real(self.view.periodic_clock(clock).period_seconds()),
                1,
            ));
        }
        let variable = coordinate_variable(coordinate).ok_or_else(|| {
            failure(
                NumericEvaluationErrorKind::NonStaticCoordinate,
                "numeric evaluation depends on a runtime coordinate",
                span,
            )
        })?;
        self.parameter_value(variable)
    }

    fn comprehension(
        &mut self,
        domain: dae::DomainId<'dae>,
        body: dae::ExprId<'dae>,
        span: Span,
    ) -> Result<Vec<Scalar>, NumericEvaluationError> {
        let domain_view = self.view.domain(domain).ok_or_else(|| {
            failure(
                NumericEvaluationErrorKind::UnsupportedOperation,
                "comprehension domain identity does not resolve",
                span,
            )
        })?;
        let mut values = Vec::new();
        for point_index in 0..domain_view.scalar_count() as usize {
            let point = domain_view
                .structured()
                .index_tuple_at(point_index)
                .map_err(|_| {
                    failure(
                        NumericEvaluationErrorKind::Overflow,
                        "comprehension domain projection overflowed",
                        span,
                    )
                })?
                .ok_or_else(|| {
                    failure(
                        NumericEvaluationErrorKind::OutOfBounds,
                        "comprehension domain point does not resolve",
                        span,
                    )
                })?;
            self.domain_points.push((domain, point));
            let scope = self
                .scoped_expression_values
                .last()
                .and_then(|frame| frame.scope);
            self.scoped_expression_values
                .push(function_states::NumericFunctionFrame::lexical_frame(scope));
            let body_values = self.source_expression(body);
            self.scoped_expression_values.pop();
            self.domain_points.pop();
            values.extend(body_values?);
        }
        Ok(values)
    }

    fn function_fold(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        carried: u32,
        definition: dae::FunctionDefinitionView<'dae>,
        span: Span,
    ) -> Result<SourceValues, NumericEvaluationError> {
        if let Some(values) = self
            .scoped_expression_values
            .last()
            .and_then(|frame| frame.folds.get(&fold))
            .or_else(|| {
                self.enclosing_definition_frame(definition, |frame| {
                    frame.folds.contains_key(&fold)
                })?
                .folds
                .get(&fold)
            })
        {
            return values
                .get(carried as usize)
                .cloned()
                .ok_or_else(|| function_ordinal_error(span));
        }
        self.function_fold_values(fold, None, span)?
            .get(carried as usize)
            .cloned()
            .ok_or_else(|| function_ordinal_error(span))
    }

    fn function_fold_values(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        statements: Option<dae::FunctionStatements<'dae>>,
        span: Span,
    ) -> Result<Vec<SourceValues>, NumericEvaluationError> {
        let fold_view = self.view.function_fold(fold).ok_or_else(|| {
            failure(
                NumericEvaluationErrorKind::UnsupportedOperation,
                "function loop identity does not resolve",
                span,
            )
        })?;
        let mut values = fold_view
            .initial_values()
            .rhs_iter()
            .map(|initial| self.source_values(initial))
            .collect::<Result<Vec<_>, _>>()?;
        let domain = self
            .view
            .domain(fold_view.domain())
            .expect("checked function loop domain resolves");
        for point_index in 0..domain.scalar_count() as usize {
            let point = function_loop_point(domain, point_index, span)?;
            self.function_fold_values.push((fold, values));
            self.domain_points.push((fold_view.domain(), point));
            self.scoped_expression_values.push(
                function_states::NumericFunctionFrame::lexical_frame(Some(fold_view.body_scope())),
            );
            // MLS §11.2.3: a bounded `while` stops before the first pass whose
            // condition is false, with the values that pass would have read.
            let next = match fold_view.continuation() {
                Some(condition) => self
                    .source_expression(condition)
                    .and_then(|value| match value.as_slice() {
                        [Scalar::Boolean(false)] => Ok(None),
                        [Scalar::Boolean(true)] => self
                            .function_loop_iteration(fold_view, statements.clone())
                            .map(Some),
                        _ => Err(failure(
                            NumericEvaluationErrorKind::InvalidValue,
                            "while condition is not scalar Boolean",
                            span,
                        )),
                    }),
                None => self
                    .function_loop_iteration(fold_view, statements.clone())
                    .map(Some),
            };
            self.scoped_expression_values.pop();
            self.domain_points.pop();
            let (_, previous) = self
                .function_fold_values
                .pop()
                .expect("active function fold stack remains balanced");
            match next? {
                Some(next) => {
                    debug_assert_eq!(next.len(), previous.len());
                    values = next;
                }
                None => {
                    values = previous;
                    break;
                }
            }
        }
        Ok(values)
    }

    fn function_call(
        &mut self,
        owner: dae::ExprId<'dae>,
        function: dae::FunctionId<'dae>,
        output: u32,
        arguments: dae::ExpressionOperands<'dae>,
        span: Span,
    ) -> Result<SourceValues, NumericEvaluationError> {
        if let Some(tuple) = self
            .scoped_expression_values
            .last()
            .and_then(|frame| frame.calls.get(&owner.index()))
        {
            return tuple
                .get(output as usize)
                .cloned()
                .ok_or_else(|| function_result_error(span));
        }
        if self.function_arguments.len() >= 256 {
            return Err(failure(
                NumericEvaluationErrorKind::CyclicDependency,
                "function evaluation exceeded the checked recursion limit",
                span,
            ));
        }
        let arguments = arguments
            .iter()
            .map(|argument| self.source_values(argument))
            .collect::<Result<Vec<_>, _>>()?;
        let definition = self.view.function(function).ok_or_else(|| {
            failure(
                NumericEvaluationErrorKind::UnsupportedOperation,
                "function identity does not resolve",
                span,
            )
        })?;
        // MLS §12.9 external bodies are foreign code. Numeric evaluation owns
        // no runtime that can execute one: only a SPEC_0040 DAE-C30 native
        // body executes, and any other external body fails with the call's
        // exact provenance instead of substituting a plausible value.
        if let Some(external) = definition.external() {
            let Some(binding) = external.native_body() else {
                return Err(external_function_failure(definition.name(), external, span));
            };
            self.function_arguments.push((function, arguments));
            self.scoped_expression_values
                .push(function_states::NumericFunctionFrame::function_entry());
            let inputs = binding
                .inputs()
                .map(|input| self.source_values(input))
                .collect::<Result<Vec<_>, _>>();
            self.scoped_expression_values.pop();
            self.function_arguments.pop();
            let tuple = native_function_call(binding, &inputs?, span)?;
            let value = tuple
                .get(output as usize)
                .cloned()
                .ok_or_else(|| function_result_error(span))?;
            self.scoped_expression_values
                .last_mut()
                .expect("numeric source frame exists")
                .calls
                .insert(owner.index(), tuple);
            return Ok(value);
        }
        self.function_arguments.push((function, arguments));
        self.scoped_expression_values
            .push(function_states::NumericFunctionFrame::function_entry());
        let tuple = self
            .function_statements(definition.statements())
            .and_then(|()| {
                definition
                    .result_values()
                    .rhs_iter()
                    .map(|result| self.source_values(result))
                    .collect::<Result<Vec<_>, _>>()
            });
        self.scoped_expression_values.pop();
        self.function_arguments.pop();
        let tuple = tuple?;
        let value = tuple
            .get(output as usize)
            .cloned()
            .ok_or_else(|| function_result_error(span))?;
        self.scoped_expression_values
            .last_mut()
            .expect("numeric source frame exists")
            .calls
            .insert(owner.index(), tuple);
        Ok(value)
    }

    fn function_statements(
        &mut self,
        statements: dae::FunctionStatements<'dae>,
    ) -> Result<(), NumericEvaluationError> {
        for statement in statements {
            match statement {
                dae::FunctionStatementView::Assignment { definition } => {
                    self.source_values(definition.rhs())?;
                }
                dae::FunctionStatementView::AssignmentGroup { definitions, .. } => {
                    self.function_assignment_group(definitions)?;
                }
                dae::FunctionStatementView::Conditional { conditional } => {
                    self.ordered_function_conditional(conditional)?
                }
                dae::FunctionStatementView::RequireAssigned {
                    conditional,
                    value,
                    definition,
                    provenance,
                } => {
                    self.require_function_assigned(
                        conditional,
                        value,
                        definition,
                        provenance.span(),
                    )?;
                }
                // A warning-level assertion never aborts an evaluation and has
                // no influence on any value (MLS §8.3.7).
                dae::FunctionStatementView::Assertion {
                    condition,
                    message,
                    level: dae::AssertionLevel::Warning,
                    ..
                } => match self.source_expression(condition)?.as_slice() {
                    [Scalar::Boolean(true)] => {}
                    [Scalar::Boolean(false)] => {
                        self.assertion_message(message)?;
                    }
                    _ => {
                        return Err(failure(
                            NumericEvaluationErrorKind::InvalidValue,
                            "function assertion condition is not scalar Boolean",
                            self.expression_span(condition),
                        ));
                    }
                },
                dae::FunctionStatementView::Assertion {
                    condition,
                    message,
                    level: dae::AssertionLevel::Error,
                    provenance,
                } => self.function_assertion(condition, message, provenance.span())?,
                dae::FunctionStatementView::For {
                    fold,
                    statements,
                    provenance,
                } => self.function_loop_statements(fold, statements, provenance.span())?,
            }
        }
        Ok(())
    }

    fn function_assertion(
        &mut self,
        condition: dae::ExprId<'dae>,
        message: dae::ExprId<'dae>,
        span: Span,
    ) -> Result<(), NumericEvaluationError> {
        match self.source_expression(condition)?.as_slice() {
            [Scalar::Boolean(true)] => Ok(()),
            [Scalar::Boolean(false)] => Err(failure(
                NumericEvaluationErrorKind::AssertionFailed,
                self.assertion_message(message)?,
                span,
            )),
            _ => Err(failure(
                NumericEvaluationErrorKind::InvalidValue,
                "function assertion condition is not scalar Boolean",
                span,
            )),
        }
    }

    fn assertion_message(
        &self,
        expression: dae::ExprId<'dae>,
    ) -> Result<String, NumericEvaluationError> {
        match self
            .view
            .expression(expression)
            .map(|node| node.operation())
        {
            Some(dae::ExpressionOperation::Literal(dae::DaeLiteral::String(message))) => {
                Ok(message.clone())
            }
            _ => Err(failure(
                NumericEvaluationErrorKind::UnsupportedOperation,
                "source assertion message execution is outside the numerical evaluator's String profile",
                self.expression_span(expression),
            )),
        }
    }

    fn function_loop_statements(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        statements: dae::FunctionStatements<'dae>,
        span: Span,
    ) -> Result<(), NumericEvaluationError> {
        let values = self.function_fold_values(fold, Some(statements), span)?;
        self.scoped_expression_values
            .last_mut()
            .expect("numeric source frame exists")
            .folds
            .insert(fold, values);
        Ok(())
    }

    fn function_loop_iteration(
        &mut self,
        fold: dae::FunctionFoldView<'dae>,
        statements: Option<dae::FunctionStatements<'dae>>,
    ) -> Result<Vec<SourceValues>, NumericEvaluationError> {
        if let Some(statements) = statements {
            self.function_statements(statements)?;
        }
        fold.update_values()
            .rhs_iter()
            .map(|update| self.source_values(update))
            .collect()
    }

    /// Evaluate the constructor-proven initial value of a variable.
    /// Preserve source fill/broadcast runs before any extent-sized allocation.
    pub fn source_initial_values(
        &mut self,
        id: dae::VariableId<'dae>,
    ) -> Result<SourceValues, NumericEvaluationError> {
        let variable = self
            .view
            .variable(id)
            .expect("finalized variable identity resolves");
        if variable.scalar_count() == 0 {
            return Ok(NumericInitialValues::default());
        }
        if matches!(
            variable.role(),
            dae::VariableRole::Parameter | dae::VariableRole::Constant
        ) {
            return self.parameter_value(id);
        }
        let expression = match variable.role() {
            dae::VariableRole::Input => {
                return self.input_value(variable);
            }
            dae::VariableRole::State
            | dae::VariableRole::Algebraic
            | dae::VariableRole::Output
            | dae::VariableRole::DiscreteReal
            | dae::VariableRole::DiscreteValue => {
                declaration_initial_expression(variable, self.input_policy).ok_or_else(|| {
                    failure(
                        NumericEvaluationErrorKind::MissingValue,
                        format!(
                            "{} `{}` has no constructor-proven initial value",
                            role_name(variable.role()),
                            variable.name()
                        ),
                        variable.declaration().span(),
                    )
                })?
            }
            dae::VariableRole::Parameter | dae::VariableRole::Constant => {
                unreachable!("static roles returned above")
            }
        };
        self.variable_expression(variable, expression)
    }

    fn input_value(
        &mut self,
        variable: dae::VariableView<'dae>,
    ) -> Result<SourceValues, NumericEvaluationError> {
        if self.input_policy == InputInitializationPolicy::FmiExportDefaults {
            return self
                .input_initialization(variable.id())
                .map(|values| values.effective().clone());
        }
        if variable.scalar_count() == 0 {
            return Ok(NumericInitialValues::default());
        }
        if let Some(expression) = declaration_initial_expression(variable, self.input_policy) {
            return self.variable_expression(variable, expression);
        }
        let mut values = Vec::with_capacity(variable.scalar_count());
        for scalar in 0..variable.scalar_count() {
            let value = (self.override_value)(variable, scalar).ok_or_else(|| {
                failure(
                    NumericEvaluationErrorKind::MissingValue,
                    format!(
                        "input `{}` has neither a checked default nor a runtime value",
                        variable
                            .scalar_name(scalar)
                            .expect("checked scalar ordinal has a name")
                    ),
                    variable.declaration().span(),
                )
            })?;
            if !value.is_finite() {
                return Err(failure(
                    NumericEvaluationErrorKind::InvalidOverride,
                    format!(
                        "runtime input `{}` must be finite",
                        variable
                            .scalar_name(scalar)
                            .expect("checked scalar ordinal has a name")
                    ),
                    variable.declaration().span(),
                ));
            }
            values.push(host_scalar(variable, value)?);
        }
        Ok(NumericInitialValues::literal(values))
    }

    fn parameter_value(
        &mut self,
        id: dae::VariableId<'dae>,
    ) -> Result<SourceValues, NumericEvaluationError> {
        let index = id.index() as usize;
        if let Some(value) = &self.values[index] {
            return Ok(value.clone());
        }
        let variable = self
            .view
            .variable(id)
            .expect("finalized parameter identity resolves");
        if variable.scalar_count() == 0 {
            self.values[index] = Some(NumericInitialValues::default());
            return Ok(NumericInitialValues::default());
        }
        if !matches!(
            variable.role(),
            dae::VariableRole::Parameter | dae::VariableRole::Constant
        ) {
            return Err(failure(
                NumericEvaluationErrorKind::NonStaticCoordinate,
                format!(
                    "runtime {} `{}` cannot be used as a static coordinate",
                    role_name(variable.role()),
                    variable.name()
                ),
                variable.declaration().span(),
            ));
        }
        if self.evaluating[index] {
            return Err(failure(
                NumericEvaluationErrorKind::CyclicDependency,
                format!(
                    "cyclic static-value dependency includes `{}`",
                    variable.name()
                ),
                variable.declaration().span(),
            ));
        }
        let expression =
            declaration_initial_expression(variable, self.input_policy).ok_or_else(|| {
                failure(
                    NumericEvaluationErrorKind::MissingValue,
                    format!(
                        "{} `{}` has neither a binding nor an initial value",
                        role_name(variable.role()),
                        variable.name()
                    ),
                    variable.declaration().span(),
                )
            })?;
        self.evaluating[index] = true;
        let result = self.variable_expression(variable, expression);
        self.evaluating[index] = false;
        let value = result?;
        self.values[index] = Some(value.clone());
        Ok(value)
    }

    fn variable_expression(
        &mut self,
        variable: dae::VariableView<'dae>,
        expression: dae::ExprId<'dae>,
    ) -> Result<SourceValues, NumericEvaluationError> {
        let mut values = self.variable_source_expression(variable, expression)?;
        self.apply_overrides(variable, &mut values)?;
        require_finite(&values, self.expression_span(expression))?;
        Ok(values)
    }

    fn variable_source_expression(
        &mut self,
        variable: dae::VariableView<'dae>,
        expression: dae::ExprId<'dae>,
    ) -> Result<SourceValues, NumericEvaluationError> {
        if variable.scalar_count() == 0 {
            return Ok(SourceValues::default());
        }
        let mut values = self.source_values(expression)?;
        if variable.value_type().scalar_type() == dae::ScalarType::Real {
            values = values.map(|value| Scalar::Real(value.real()));
        }
        values.broadcast(variable.scalar_count());
        if values.len() != variable.scalar_count() {
            return Err(failure(
                NumericEvaluationErrorKind::ShapeMismatch,
                format!(
                    "value for `{}` contains {} scalars; expected {}",
                    variable.name(),
                    values.len(),
                    variable.scalar_count()
                ),
                self.expression_span(expression),
            ));
        }
        Ok(values)
    }

    fn apply_overrides(
        &mut self,
        variable: dae::VariableView<'dae>,
        values: &mut SourceValues,
    ) -> Result<(), NumericEvaluationError> {
        let overrides = self.checked_overrides(variable, values.len())?;
        values.apply_source_overrides(&overrides);
        Ok(())
    }

    fn checked_overrides(
        &mut self,
        variable: dae::VariableView<'dae>,
        count: usize,
    ) -> Result<Vec<(usize, Scalar)>, NumericEvaluationError> {
        let mut overrides = Vec::new();
        for scalar in 0..count {
            let Some(override_value) = (self.override_value)(variable, scalar) else {
                continue;
            };
            let name = variable
                .scalar_name(scalar)
                .expect("checked scalar ordinal has a name");
            if variable.role() != dae::VariableRole::Input && !variable.is_tunable() {
                return Err(failure(
                    NumericEvaluationErrorKind::InvalidOverride,
                    format!(
                        "`{name}` is not a tunable parameter; change structural values by recompiling"
                    ),
                    variable.declaration().span(),
                ));
            }
            if !override_value.is_finite() {
                return Err(failure(
                    NumericEvaluationErrorKind::InvalidOverride,
                    format!("override for `{name}` must be finite"),
                    variable.declaration().span(),
                ));
            }
            overrides.push((scalar, host_scalar(variable, override_value)?));
        }
        Ok(overrides)
    }

    fn conditional(
        &mut self,
        operands: dae::ExpressionOperands<'dae>,
        span: Span,
    ) -> Result<SourceValues, NumericEvaluationError> {
        for ordinal in (0..operands.len() - 1).step_by(2) {
            let condition = self.source_expression(
                operands
                    .get(ordinal)
                    .expect("checked conditional condition resolves"),
            )?;
            if condition.as_slice() == [Scalar::Boolean(true)] {
                return self.source_values(
                    operands
                        .get(ordinal + 1)
                        .expect("checked conditional value resolves"),
                );
            }
            if condition.as_slice() != [Scalar::Boolean(false)] {
                return Err(failure(
                    NumericEvaluationErrorKind::InvalidValue,
                    "conditional guard is not scalar Boolean",
                    span,
                ));
            }
        }
        self.source_values(
            operands
                .get(operands.len() - 1)
                .expect("checked conditional fallback resolves"),
        )
    }

    fn binary_expression(
        &mut self,
        operator: dae::BinaryOperator,
        lhs_id: dae::ExprId<'dae>,
        rhs_id: dae::ExprId<'dae>,
        span: Span,
    ) -> Result<Vec<Scalar>, NumericEvaluationError> {
        let lhs_dimensions = self
            .view
            .expression(lhs_id)
            .expect("checked binary lhs resolves")
            .value_type()
            .dimensions();
        let rhs_dimensions = self
            .view
            .expression(rhs_id)
            .expect("checked binary rhs resolves")
            .value_type()
            .dimensions();
        let lhs = self.source_expression(lhs_id)?;
        let rhs = self.source_expression(rhs_id)?;
        if operator == dae::BinaryOperator::Multiply {
            return multiply_values(&lhs, &rhs, lhs_dimensions, rhs_dimensions, span);
        }
        if operator == dae::BinaryOperator::Power && !lhs_dimensions.is_empty() {
            return Err(failure(
                NumericEvaluationErrorKind::UnsupportedOperation,
                "matrix power does not yet have checked numeric lowering",
                span,
            ));
        }
        let count = lhs.len().max(rhs.len());
        if (lhs.len() != count && lhs.len() != 1) || (rhs.len() != count && rhs.len() != 1) {
            return Err(failure(
                NumericEvaluationErrorKind::ShapeMismatch,
                "binary numeric-value shape mismatch",
                span,
            ));
        }
        Ok((0..count)
            .map(|index| {
                source_binary(
                    operator,
                    lhs[if lhs.len() == 1 { 0 } else { index }],
                    rhs[if rhs.len() == 1 { 0 } else { index }],
                )
            })
            .collect())
    }

    fn index(
        &mut self,
        base: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        _result_type: &dae::ValueType,
        span: Span,
    ) -> Result<Vec<Scalar>, NumericEvaluationError> {
        let base_node = self
            .view
            .expression(base)
            .expect("checked index base resolves");
        let selected = self.selected_scalar_slots(base_node, subscripts, span)?;
        let base = self.source_expression(base)?;
        selected
            .into_iter()
            .map(|flat| {
                base.get(flat).copied().ok_or_else(|| {
                    failure(
                        NumericEvaluationErrorKind::OutOfBounds,
                        "checked array selection did not resolve",
                        span,
                    )
                })
            })
            .collect()
    }

    fn array_update(
        &mut self,
        base: dae::ExprId<'dae>,
        value: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        span: Span,
    ) -> Result<Vec<Scalar>, NumericEvaluationError> {
        let base_node = self
            .view
            .expression(base)
            .expect("checked array-update base resolves");
        let targets = self.selected_scalar_slots(base_node, subscripts, span)?;
        let updated = self.source_expression(value)?;
        if updated.len() != targets.len() {
            return Err(failure(
                NumericEvaluationErrorKind::ShapeMismatch,
                format!(
                    "array update selects {} values but received {}",
                    targets.len(),
                    updated.len()
                ),
                span,
            ));
        }
        let mut result = self.source_expression(base)?;
        for (flat, value) in targets.into_iter().zip(updated) {
            let target = result.get_mut(flat).ok_or_else(|| {
                failure(
                    NumericEvaluationErrorKind::OutOfBounds,
                    "checked array update did not resolve",
                    span,
                )
            })?;
            *target = value;
        }
        Ok(result)
    }

    /// Positions, in the flattened value vector, that a subscript selects.
    ///
    /// `selected_flat_indices` counts array *elements*, which is the same
    /// thing as a scalar position only while the element is one scalar wide.
    /// A record array is stored flat -- element `k` of `Pair[2]` occupies
    /// scalars `2k..2k+2`, not scalar `k` -- so selecting an element of one
    /// has to widen each index into the record's packed run. Without this,
    /// `cs[1]` on a `Candidate[2]` yielded a single scalar where the checked
    /// field layout expects a whole record, which is the EX002 that made
    /// every record-array element assignment unusable.
    fn selected_scalar_slots(
        &mut self,
        base_node: dae::ExpressionView<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        span: Span,
    ) -> Result<Vec<usize>, NumericEvaluationError> {
        let elements = self.selected_flat_indices(base_node, subscripts, span)?;
        let width = self.element_scalar_width(base_node, span)?;
        if width == 1 {
            return Ok(elements);
        }
        let mut slots = Vec::with_capacity(elements.len().saturating_mul(width));
        for element in elements {
            let start = element
                .checked_mul(width)
                .ok_or_else(|| invalid_record_layout(span))?;
            for offset in 0..width {
                slots.push(
                    start
                        .checked_add(offset)
                        .ok_or_else(|| invalid_record_layout(span))?,
                );
            }
        }
        Ok(slots)
    }

    /// Packed scalar width of one element of an expression's value type.
    ///
    /// Non-record elements are one scalar wide. A record element is as wide as
    /// its packed field layout, which already accounts for array-typed and
    /// nested-record fields.
    fn element_scalar_width(
        &self,
        base_node: dae::ExpressionView<'dae>,
        span: Span,
    ) -> Result<usize, NumericEvaluationError> {
        if !base_node.value_type().is_record() {
            return Ok(1);
        }
        if base_node.value_type().record_field_count() == 0 {
            return Ok(0);
        }
        self.view
            .record_field_layout(base_node.value_type_id(), 0)
            .map(dae::RecordFieldLayout::record_width)
            .ok_or_else(|| invalid_record_layout(span))
    }

    fn selected_flat_indices(
        &mut self,
        base_node: dae::ExpressionView<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        span: Span,
    ) -> Result<Vec<usize>, NumericEvaluationError> {
        let dimensions = base_node.value_type().dimensions();
        if subscripts.len() != dimensions.len() {
            return Err(failure(
                NumericEvaluationErrorKind::UnsupportedOperation,
                "numeric evaluation requires one subscript per array dimension",
                span,
            ));
        }

        let mut flats = vec![0usize];
        for (axis, extent) in dimensions.iter().copied().enumerate() {
            let selected = match subscripts
                .get(axis)
                .expect("checked update has one subscript per selected axis")
            {
                dae::SubscriptView::Index {
                    expression,
                    provenance,
                } => self.validated_indices(expression, extent, provenance.span())?,
                dae::SubscriptView::Whole { .. } => (0..extent as usize).collect(),
                dae::SubscriptView::Slice {
                    expression,
                    provenance,
                } => self.validated_indices(expression, extent, provenance.span())?,
            };

            flats = expand_flat_indices(flats, &selected, extent, span)?;
        }
        Ok(flats)
    }

    fn validated_indices(
        &mut self,
        expression: dae::ExprId<'dae>,
        extent: u32,
        span: Span,
    ) -> Result<Vec<usize>, NumericEvaluationError> {
        self.source_expression(expression)?
            .into_iter()
            .map(|index| {
                let exact = index
                    .integer()
                    .and_then(|value| usize::try_from(value).ok());
                if !exact.is_some_and(|value| value >= 1 && value <= extent as usize) {
                    return Err(failure(
                        NumericEvaluationErrorKind::OutOfBounds,
                        format!("array index {index} is outside 1..={extent}"),
                        span,
                    ));
                }
                Ok(exact.expect("checked source index") - 1)
            })
            .collect()
    }

    fn builtin(
        &mut self,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'dae>,
        result_dimensions: &[u32],
        span: Span,
    ) -> Result<SourceValues, NumericEvaluationError> {
        if let Some(values) =
            self.array_constructor_builtin(builtin, arguments, result_dimensions, span)?
        {
            return Ok(values);
        }
        let first = arguments.get(0).expect("checked builtin operand");
        let mut values = self.source_expression(first)?;
        use dae::PureBuiltin as B;
        match builtin {
            dae::PureBuiltin::Abs => values.iter_mut().for_each(|value| *value = value.abs()),
            dae::PureBuiltin::Sign => values.iter_mut().for_each(|v| *v = v.sign()),
            dae::PureBuiltin::Sqrt => values.iter_mut().for_each(|value| *value = value.sqrt()),
            dae::PureBuiltin::Div | dae::PureBuiltin::Mod | dae::PureBuiltin::Rem => {
                let rhs = self.source_expression(
                    arguments
                        .get(1)
                        .expect("checked quotient builtin has two arguments"),
                )?;
                apply_quotient(builtin, &mut values, rhs, span)?;
            }
            dae::PureBuiltin::Floor => values.iter_mut().for_each(|value| *value = value.floor()),
            dae::PureBuiltin::Ceil => values.iter_mut().for_each(|value| *value = value.ceil()),
            dae::PureBuiltin::Integer => values
                .iter_mut()
                .for_each(|value| *value = value.integer_builtin()),
            dae::PureBuiltin::Sin => values.iter_mut().for_each(|value| *value = value.sin()),
            dae::PureBuiltin::Cos => values.iter_mut().for_each(|value| *value = value.cos()),
            dae::PureBuiltin::Tan => values.iter_mut().for_each(|value| *value = value.tan()),
            dae::PureBuiltin::Asin => values.iter_mut().for_each(|value| *value = value.asin()),
            dae::PureBuiltin::Acos => values.iter_mut().for_each(|value| *value = value.acos()),
            dae::PureBuiltin::Atan => values.iter_mut().for_each(|value| *value = value.atan()),
            dae::PureBuiltin::Atan2 => {
                let rhs = self.source_expression(
                    arguments.get(1).expect("checked atan2 has two arguments"),
                )?;
                for (lhs, rhs) in values.iter_mut().zip(rhs) {
                    *lhs = lhs.atan2(rhs);
                }
            }
            dae::PureBuiltin::Sinh => values.iter_mut().for_each(|value| *value = value.sinh()),
            dae::PureBuiltin::Cosh => values.iter_mut().for_each(|value| *value = value.cosh()),
            dae::PureBuiltin::Tanh => values.iter_mut().for_each(|value| *value = value.tanh()),
            dae::PureBuiltin::Exp => values.iter_mut().for_each(|value| *value = value.exp()),
            dae::PureBuiltin::Log => values.iter_mut().for_each(|value| *value = value.ln()),
            dae::PureBuiltin::Log10 => values.iter_mut().for_each(|value| *value = value.log10()),
            dae::PureBuiltin::Smooth => {
                values = self
                    .source_expression(arguments.get(1).expect("checked smooth value argument"))?;
            }
            dae::PureBuiltin::NoEvent => {}
            dae::PureBuiltin::Homotopy => {}
            dae::PureBuiltin::Vector => {}
            dae::PureBuiltin::LinearSolve => {
                return Err(failure(
                    NumericEvaluationErrorKind::UnsupportedOperation,
                    "linear solve requires the Solve tensor execution kernel",
                    span,
                ));
            }
            dae::PureBuiltin::Transpose => values = transpose_values(&values, result_dimensions),
            B::Diagonal | B::OuterProduct | B::Skew => {
                values = self.matrix_product(builtin, arguments, values)?;
            }
            dae::PureBuiltin::Sum => values = vec![values.iter().sum()],
            dae::PureBuiltin::Product => values = vec![values.iter().product()],
            dae::PureBuiltin::Min | dae::PureBuiltin::Max => {
                values = self.extremum(builtin, arguments, values, span)?;
            }
            dae::PureBuiltin::Size => unreachable!("size returns before operand evaluation"),
            dae::PureBuiltin::Zeros
            | dae::PureBuiltin::Ones
            | dae::PureBuiltin::Fill
            | dae::PureBuiltin::Linspace
            | dae::PureBuiltin::Cross
            | dae::PureBuiltin::PromotedCat1
            | dae::PureBuiltin::PromotedCat2
            | dae::PureBuiltin::Identity => {
                unreachable!("array constructors return before operand evaluation")
            }
        }
        Ok(NumericInitialValues::literal(values))
    }

    fn array_constructor_builtin(
        &mut self,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'dae>,
        result_dimensions: &[u32],
        span: Span,
    ) -> Result<Option<SourceValues>, NumericEvaluationError> {
        let values = match builtin {
            dae::PureBuiltin::PromotedCat1 | dae::PureBuiltin::PromotedCat2 => {
                let axis = usize::from(builtin == dae::PureBuiltin::PromotedCat2);
                self.promoted_concatenation(arguments, axis, result_dimensions, span)?
            }
            dae::PureBuiltin::Identity => identity_values(result_dimensions),
            dae::PureBuiltin::Zeros | dae::PureBuiltin::Ones => {
                let value = if builtin == dae::PureBuiltin::Ones {
                    Scalar::Integer(1)
                } else {
                    Scalar::Integer(0)
                };
                return Ok(Some(self.filled_array(arguments, 0, value, span)?));
            }
            dae::PureBuiltin::Fill => {
                let fill = self.source_expression(
                    arguments.get(0).expect("checked fill has a value argument"),
                )?;
                let [fill] = fill.as_slice() else {
                    unreachable!("checked fill value is scalar")
                };
                return Ok(Some(self.filled_array(arguments, 1, *fill, span)?));
            }
            dae::PureBuiltin::Linspace => self.linspace(arguments)?,
            dae::PureBuiltin::Cross => self.cross(arguments)?,
            dae::PureBuiltin::Size => {
                let first = arguments
                    .get(0)
                    .expect("checked size has an array argument");
                self.size(arguments, first, span)?
            }
            _ => return Ok(None),
        };
        Ok(Some(NumericInitialValues::literal(values)))
    }

    fn matrix_product(
        &mut self,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'dae>,
        lhs: Vec<Scalar>,
    ) -> Result<Vec<Scalar>, NumericEvaluationError> {
        match builtin {
            dae::PureBuiltin::Diagonal => Ok(diagonal_values(&lhs)),
            dae::PureBuiltin::OuterProduct => {
                let rhs = self.source_expression(
                    arguments
                        .get(1)
                        .expect("checked outerProduct has two arguments"),
                )?;
                Ok(outer_product_values(&lhs, &rhs))
            }
            dae::PureBuiltin::Skew => Ok(skew_values(&lhs)),
            _ => unreachable!("only compact matrix products use this evaluator"),
        }
    }

    fn promoted_concatenation(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        axis: usize,
        result_dimensions: &[u32],
        span: Span,
    ) -> Result<Vec<Scalar>, NumericEvaluationError> {
        let operands = arguments
            .iter()
            .map(|argument| {
                let dimensions = self
                    .view
                    .expression(argument)
                    .expect("checked concatenation operand resolves")
                    .value_type()
                    .dimensions();
                let axis_extent = dimensions.get(axis).copied().unwrap_or(1) as usize;
                self.source_expression(argument)
                    .map(|values| (axis_extent, values))
            })
            .collect::<Result<Vec<_>, _>>()?;
        let outer_count = result_dimensions[..axis]
            .iter()
            .try_fold(1_usize, |count, extent| count.checked_mul(*extent as usize))
            .ok_or_else(|| {
                failure(
                    NumericEvaluationErrorKind::Overflow,
                    "checked concatenation outer scalar count overflowed",
                    span,
                )
            })?;
        let inner_count = result_dimensions[axis + 1..]
            .iter()
            .try_fold(1_usize, |count, extent| count.checked_mul(*extent as usize))
            .ok_or_else(|| {
                failure(
                    NumericEvaluationErrorKind::Overflow,
                    "checked concatenation inner scalar count overflowed",
                    span,
                )
            })?;
        let result_count = result_dimensions
            .iter()
            .try_fold(1_usize, |count, extent| count.checked_mul(*extent as usize))
            .ok_or_else(|| {
                failure(
                    NumericEvaluationErrorKind::Overflow,
                    "checked concatenation scalar count overflowed",
                    span,
                )
            })?;
        let mut result = Vec::with_capacity(result_count);
        for outer in 0..outer_count {
            for (axis_extent, values) in &operands {
                let chunk_len = axis_extent
                    .checked_mul(inner_count)
                    .expect("checked concatenation operand scalar count fits usize");
                let start = outer
                    .checked_mul(chunk_len)
                    .expect("checked concatenation operand offset fits usize");
                let end = start
                    .checked_add(chunk_len)
                    .expect("checked concatenation operand end fits usize");
                result.extend_from_slice(
                    values
                        .get(start..end)
                        .expect("checked promoted operand has its constructed shape"),
                );
            }
        }
        debug_assert_eq!(result.len(), result_count);
        Ok(result)
    }

    fn filled_array(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        first_extent: usize,
        value: Scalar,
        span: Span,
    ) -> Result<SourceValues, NumericEvaluationError> {
        let mut count = 1_usize;
        for argument in arguments.iter().skip(first_extent) {
            let extent = self.source_expression(argument)?;
            let [extent] = extent.as_slice() else {
                return Err(failure(
                    NumericEvaluationErrorKind::ShapeMismatch,
                    "checked array-constructor extent is not scalar",
                    span,
                ));
            };
            let extent = extent
                .integer()
                .and_then(|value| usize::try_from(value).ok())
                .ok_or_else(|| {
                    failure(
                        NumericEvaluationErrorKind::InvalidValue,
                        "array extent is not a nonnegative exact Integer",
                        span,
                    )
                })?;
            if extent > isize::MAX as usize {
                return Err(failure(
                    NumericEvaluationErrorKind::InvalidValue,
                    "checked array-constructor extent is not a nonnegative integer",
                    span,
                ));
            }
            count = count.checked_mul(extent).ok_or_else(|| {
                failure(
                    NumericEvaluationErrorKind::Overflow,
                    "checked array-constructor scalar count overflowed",
                    span,
                )
            })?;
        }
        Ok(NumericInitialValues::repeat(value, count))
    }

    fn linspace(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
    ) -> Result<Vec<Scalar>, NumericEvaluationError> {
        let start = self.source_expression(arguments.get(0).expect("checked linspace start"))?[0];
        let stop = self.source_expression(arguments.get(1).expect("checked linspace stop"))?[0];
        let count = arguments.get(2).expect("checked linspace extent");
        let dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(count)) = self
            .view
            .expression(count)
            .expect("checked linspace extent resolves")
            .operation()
        else {
            unreachable!("checked linspace extent is a literal Integer")
        };
        let count = u32::try_from(*count).expect("checked linspace extent is in the u32 domain");
        let denominator = Scalar::Real(f64::from(count - 1));
        Ok((0..count)
            .map(|ordinal| start + (stop - start) * Scalar::Real(f64::from(ordinal)) / denominator)
            .collect())
    }

    fn cross(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
    ) -> Result<Vec<Scalar>, NumericEvaluationError> {
        let lhs = self.source_expression(arguments.get(0).expect("checked cross lhs"))?;
        let rhs = self.source_expression(arguments.get(1).expect("checked cross rhs"))?;
        Ok(vec![
            lhs[1] * rhs[2] - lhs[2] * rhs[1],
            lhs[2] * rhs[0] - lhs[0] * rhs[2],
            lhs[0] * rhs[1] - lhs[1] * rhs[0],
        ])
    }

    fn extremum(
        &mut self,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'dae>,
        mut values: Vec<Scalar>,
        span: Span,
    ) -> Result<Vec<Scalar>, NumericEvaluationError> {
        if arguments.len() == 1 {
            let compare = if builtin == dae::PureBuiltin::Min {
                Scalar::min
            } else {
                Scalar::max
            };
            return values
                .into_iter()
                .reduce(compare)
                .map(|value| vec![value])
                .ok_or_else(|| {
                    failure(
                        NumericEvaluationErrorKind::InvalidValue,
                        "reduction requires at least one scalar",
                        span,
                    )
                });
        }
        for argument in arguments.iter().skip(1) {
            let rhs = self.source_expression(argument)?;
            expect_same_length(&values, &rhs, span)?;
            combine_extremum(builtin, &mut values, rhs);
        }
        Ok(values)
    }

    fn size(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        first: dae::ExprId<'dae>,
        span: Span,
    ) -> Result<Vec<Scalar>, NumericEvaluationError> {
        let dimensions = self
            .view
            .expression(first)
            .expect("checked size operand resolves")
            .value_type()
            .dimensions();
        let Some(dimension) = arguments.get(1) else {
            return Ok(dimensions
                .iter()
                .map(|extent| Scalar::Integer(i64::from(*extent)))
                .collect());
        };
        let dimension = self.source_expression(dimension)?;
        let [dimension] = dimension.as_slice() else {
            return Err(failure(
                NumericEvaluationErrorKind::ShapeMismatch,
                "size dimension is not scalar",
                span,
            ));
        };
        let dimension = dimension
            .integer()
            .and_then(|value| usize::try_from(value).ok());
        if !dimension.is_some_and(|value| value >= 1 && value <= dimensions.len()) {
            return Err(failure(
                NumericEvaluationErrorKind::OutOfBounds,
                "size dimension is outside the array rank",
                span,
            ));
        }
        Ok(vec![Scalar::Integer(i64::from(
            dimensions[dimension.expect("checked dimension") - 1],
        ))])
    }

    fn expression_span(&self, expression: dae::ExprId<'dae>) -> Span {
        self.view
            .expression(expression)
            .expect("finalized expression identity resolves")
            .provenance()
            .span()
    }
}

fn invalid_record_layout(span: Span) -> NumericEvaluationError {
    failure(
        NumericEvaluationErrorKind::UnsupportedOperation,
        "record value does not match its checked field layout",
        span,
    )
}

fn apply_quotient(
    builtin: dae::PureBuiltin,
    values: &mut [Scalar],
    rhs: Vec<Scalar>,
    span: Span,
) -> Result<(), NumericEvaluationError> {
    let function = match builtin {
        dae::PureBuiltin::Div => rumoca_core::BuiltinFunction::Div,
        dae::PureBuiltin::Mod => rumoca_core::BuiltinFunction::Mod,
        dae::PureBuiltin::Rem => rumoca_core::BuiltinFunction::Rem,
        _ => unreachable!("caller restricts quotient builtins"),
    };
    for (lhs, rhs) in values.iter_mut().zip(rhs) {
        let value = match (lhs.integer(), rhs.integer()) {
            (Some(lhs), Some(rhs)) => match builtin {
                dae::PureBuiltin::Div => rumoca_core::eval_integer_div_builtin(lhs, rhs),
                dae::PureBuiltin::Mod => rumoca_core::eval_integer_mod_builtin(lhs, rhs),
                dae::PureBuiltin::Rem => rumoca_core::eval_integer_rem_builtin(lhs, rhs),
                _ => unreachable!("checked quotient builtin"),
            }
            .map(Scalar::Integer),
            _ => rumoca_core::apply_scalar_binary_math(function, lhs.real(), rhs.real())
                .map(Scalar::Real),
        };
        *lhs = value.ok_or_else(|| {
            failure(
                NumericEvaluationErrorKind::InvalidValue,
                "quotient divisor must be nonzero",
                span,
            )
        })?;
    }
    Ok(())
}

fn no_override(_variable: dae::VariableView<'_>, _scalar: usize) -> Option<f64> {
    None
}

fn host_scalar(
    variable: dae::VariableView<'_>,
    value: f64,
) -> Result<Scalar, NumericEvaluationError> {
    let payload = match variable.value_type().scalar_type() {
        dae::ScalarType::Real => Some(Scalar::Real(value)),
        dae::ScalarType::Integer | dae::ScalarType::Enumeration
            if value.is_finite()
                && value.fract() == 0.0
                && value.abs() <= 9_007_199_254_740_992.0 =>
        {
            Some(
                if variable.value_type().scalar_type() == dae::ScalarType::Integer {
                    Scalar::Integer(value as i64)
                } else {
                    Scalar::Enumeration(value as i64)
                },
            )
        }
        dae::ScalarType::Boolean if value == 0.0 || value == 1.0 => {
            Some(Scalar::Boolean(value == 1.0))
        }
        _ => None,
    };
    payload.ok_or_else(|| {
        failure(
            NumericEvaluationErrorKind::InvalidOverride,
            "numerical host override cannot represent the declared exact scalar type",
            variable.declaration().span(),
        )
    })
}

fn function_parameter_error(span: Span) -> NumericEvaluationError {
    failure(
        NumericEvaluationErrorKind::NonStaticCoordinate,
        "function parameter escaped its checked call owner",
        span,
    )
}

fn function_ordinal_error(span: Span) -> NumericEvaluationError {
    failure(
        NumericEvaluationErrorKind::OutOfBounds,
        "function parameter ordinal is out of range",
        span,
    )
}

pub(crate) fn external_function_failure(
    name: &rumoca_core::VarName,
    external: dae::ExternalFunctionView<'_>,
    span: Span,
) -> NumericEvaluationError {
    failure(
        NumericEvaluationErrorKind::ExternalFunction,
        format!(
            "external {} function `{}` calls `{}`, which this runtime cannot execute",
            external.language().as_str(),
            name,
            external.symbol()
        ),
        span,
    )
}

fn function_result_error(span: Span) -> NumericEvaluationError {
    failure(
        NumericEvaluationErrorKind::OutOfBounds,
        "function result ordinal is out of range",
        span,
    )
}

fn coordinate_variable(coordinate: dae::CoordinateView<'_>) -> Option<dae::VariableId<'_>> {
    match coordinate {
        dae::CoordinateView::Parameter(id) => Some(id.into()),
        _ => None,
    }
}

fn literal_value(literal: &dae::DaeLiteral, span: Span) -> Result<Scalar, NumericEvaluationError> {
    match literal {
        dae::DaeLiteral::Real(value) => Ok(Scalar::Real(*value)),
        dae::DaeLiteral::Integer(value) => Ok(Scalar::Integer(*value)),
        dae::DaeLiteral::Enumeration(value) => Ok(Scalar::Enumeration(*value)),
        dae::DaeLiteral::Boolean(value) => Ok(bool_value(*value)),
        dae::DaeLiteral::String(_) => Err(failure(
            NumericEvaluationErrorKind::UnsupportedOperation,
            "String cannot be evaluated as a numeric value",
            span,
        )),
    }
}

pub(crate) fn binary(operator: dae::BinaryOperator, lhs: f64, rhs: f64) -> f64 {
    source_binary(operator, Scalar::Real(lhs), Scalar::Real(rhs)).real()
}

fn source_binary(operator: dae::BinaryOperator, lhs: Scalar, rhs: Scalar) -> Scalar {
    match operator {
        dae::BinaryOperator::Add | dae::BinaryOperator::ElementwiseAdd => lhs + rhs,
        dae::BinaryOperator::Subtract | dae::BinaryOperator::ElementwiseSubtract => lhs - rhs,
        dae::BinaryOperator::Multiply | dae::BinaryOperator::ElementwiseMultiply => lhs * rhs,
        dae::BinaryOperator::Divide | dae::BinaryOperator::ElementwiseDivide => lhs / rhs,
        dae::BinaryOperator::Power | dae::BinaryOperator::ElementwisePower => lhs.powf(rhs),
        dae::BinaryOperator::Equal => bool_value(lhs == rhs),
        dae::BinaryOperator::NotEqual => bool_value(lhs != rhs),
        dae::BinaryOperator::Less => bool_value(lhs < rhs),
        dae::BinaryOperator::LessEqual => bool_value(lhs <= rhs),
        dae::BinaryOperator::Greater => bool_value(lhs > rhs),
        dae::BinaryOperator::GreaterEqual => bool_value(lhs >= rhs),
        dae::BinaryOperator::And => bool_value(lhs != 0.0 && rhs != 0.0),
        dae::BinaryOperator::Or => bool_value(lhs != 0.0 || rhs != 0.0),
    }
}

const fn bool_value(value: bool) -> Scalar {
    Scalar::Boolean(value)
}

fn require_finite(values: &SourceValues, span: Span) -> Result<(), NumericEvaluationError> {
    if values.runs().any(|run| match run {
        NumericInitialRun::Repeat { value, .. } => matches!(value, Scalar::Invalid),
        NumericInitialRun::Literal(values) => {
            values.iter().any(|value| matches!(value, Scalar::Invalid))
        }
    }) {
        return Err(failure(
            NumericEvaluationErrorKind::Overflow,
            "exact Integer operation has no supported checked result",
            span,
        ));
    }
    if values.runs().all(|run| match run {
        NumericInitialRun::Repeat { value, .. } => value.is_finite(),
        NumericInitialRun::Literal(values) => values.iter().all(|value| value.is_finite()),
    }) {
        Ok(())
    } else {
        Err(failure(
            NumericEvaluationErrorKind::InvalidValue,
            "numeric evaluation produced a non-finite result",
            span,
        ))
    }
}

const fn role_name(role: dae::VariableRole) -> &'static str {
    match role {
        dae::VariableRole::Parameter => "parameter",
        dae::VariableRole::Constant => "constant",
        dae::VariableRole::Input => "input",
        dae::VariableRole::State => "state",
        dae::VariableRole::Algebraic => "algebraic",
        dae::VariableRole::Output => "output",
        dae::VariableRole::DiscreteReal => "discrete-real",
        dae::VariableRole::DiscreteValue => "discrete-valued",
    }
}

fn failure(
    kind: NumericEvaluationErrorKind,
    message: impl Into<String>,
    span: Span,
) -> NumericEvaluationError {
    NumericEvaluationError {
        kind,
        message: message.into(),
        span,
    }
}
