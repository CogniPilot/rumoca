use rumoca_core::Span;
use rumoca_ir_dae as dae;

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
/// bindings. Runtime inputs require either a checked default binding or an
/// override for every scalar. Every other coordinate is rejected: this
/// evaluator never invents a runtime value. The optional override function is
/// applied at the variable boundary, so dependents observe the same overridden
/// values as the runtime layout.
pub struct NumericEvaluator<'dae, F = fn(dae::VariableView<'dae>, usize) -> Option<f64>> {
    view: dae::DaeView<'dae>,
    values: Vec<Option<Vec<f64>>>,
    evaluating: Vec<bool>,
    function_arguments: Vec<(dae::FunctionId<'dae>, Vec<Vec<f64>>)>,
    function_fold_values: Vec<(dae::FunctionFoldId<'dae>, Vec<Vec<f64>>)>,
    domain_points: Vec<(dae::DomainId<'dae>, Vec<i64>)>,
    override_value: F,
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
    pub fn with_overrides(view: dae::DaeView<'dae>, override_value: F) -> Self {
        Self {
            view,
            values: vec![None; view.variable_count()],
            evaluating: vec![false; view.variable_count()],
            function_arguments: Vec::new(),
            function_fold_values: Vec::new(),
            domain_points: Vec::new(),
            override_value,
        }
    }

    /// Evaluate an expression whose only coordinates are static parameters.
    pub fn expression(
        &mut self,
        id: dae::ExprId<'dae>,
    ) -> Result<Vec<f64>, NumericEvaluationError> {
        let node = self
            .view
            .expression(id)
            .expect("finalized expression identity resolves");
        let span = node.provenance().span();
        let value = match node.operation() {
            dae::ExpressionOperation::Literal(literal) => vec![literal_value(literal, span)?],
            dae::ExpressionOperation::Coordinate(coordinate) => {
                self.coordinate_value(coordinate, span)?
            }
            dae::ExpressionOperation::Unary { operator, operand } => self
                .expression(operand)?
                .into_iter()
                .map(|value| match operator {
                    dae::UnaryOperator::Plus => value,
                    dae::UnaryOperator::Negate => -value,
                    dae::UnaryOperator::Not => bool_value(value == 0.0),
                })
                .collect(),
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                self.binary_expression(operator, lhs, rhs, span)?
            }
            dae::ExpressionOperation::Conditional(operands) => self.conditional(operands, span)?,
            dae::ExpressionOperation::Array(elements) => {
                let mut values = Vec::new();
                for element in elements.iter() {
                    values.extend(self.expression(element)?);
                }
                values
            }
            dae::ExpressionOperation::Record(fields) => self.record(fields)?,
            dae::ExpressionOperation::Field { base, field } => {
                self.record_field(base, field as usize, span)?
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
                self.builtin(builtin, arguments, node.value_type().dimensions(), span)?
            }
            dae::ExpressionOperation::Comprehension { domain, body } => {
                self.comprehension(domain, body, span)?
            }
            dae::ExpressionOperation::Call {
                function,
                output,
                arguments,
            } => self.function_call(function, output, arguments, span)?,
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                self.expression(definition.rhs())?
            }
            dae::ExpressionOperation::FunctionFoldParameter { fold, carried, .. } => self
                .function_fold_values
                .last()
                .filter(|(active, _)| *active == fold)
                .and_then(|(_, values)| values.get(carried as usize))
                .cloned()
                .ok_or_else(|| {
                    failure(
                        NumericEvaluationErrorKind::UnsupportedOperation,
                        "function loop transition parameter escaped its fold",
                        span,
                    )
                })?,
            dae::ExpressionOperation::FunctionFoldOutput { fold, carried, .. } => {
                self.function_fold(fold, carried, span)?
            }
            dae::ExpressionOperation::StringConversion { .. } => {
                return Err(failure(
                    NumericEvaluationErrorKind::UnsupportedOperation,
                    "String conversion is outside the numeric DAE evaluator",
                    span,
                ));
            }
        };
        require_finite(&value, span)?;
        Ok(value)
    }

    fn record(
        &mut self,
        fields: dae::ExpressionOperands<'dae>,
    ) -> Result<Vec<f64>, NumericEvaluationError> {
        let mut values = Vec::new();
        for field in fields.iter() {
            values.extend(self.expression(field)?);
        }
        Ok(values)
    }

    fn record_field(
        &mut self,
        base: dae::ExprId<'dae>,
        field: usize,
        span: Span,
    ) -> Result<Vec<f64>, NumericEvaluationError> {
        let base_node = self
            .view
            .expression(base)
            .expect("checked record base resolves");
        let (start, count) = record_field_bounds(self.view, base_node.value_type_id(), field)
            .ok_or_else(|| {
                failure(
                    NumericEvaluationErrorKind::UnsupportedOperation,
                    "record field layout is not finite",
                    span,
                )
            })?;
        self.expression(base)?
            .get(start..start + count)
            .ok_or_else(|| {
                failure(
                    NumericEvaluationErrorKind::UnsupportedOperation,
                    "record value does not match its checked field layout",
                    span,
                )
            })
            .map(<[f64]>::to_vec)
    }

    fn coordinate_value(
        &mut self,
        coordinate: dae::CoordinateView<'dae>,
        span: Span,
    ) -> Result<Vec<f64>, NumericEvaluationError> {
        if let dae::CoordinateView::FunctionParameter(parameter) = coordinate {
            let arguments = self
                .function_arguments
                .last()
                .filter(|(function, _)| *function == parameter.function())
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
                .map(|value| vec![*value as f64])
                .ok_or_else(|| {
                    failure(
                        NumericEvaluationErrorKind::UnsupportedOperation,
                        "domain binder escaped its checked owner",
                        span,
                    )
                });
        }
        if let dae::CoordinateView::ClockInterval(clock) = coordinate {
            return Ok(vec![self.view.periodic_clock(clock).period_seconds()]);
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
    ) -> Result<Vec<f64>, NumericEvaluationError> {
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
            let body_values = self.expression(body);
            self.domain_points.pop();
            values.extend(body_values?);
        }
        Ok(values)
    }

    fn function_fold(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        carried: u32,
        span: Span,
    ) -> Result<Vec<f64>, NumericEvaluationError> {
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
    ) -> Result<Vec<Vec<f64>>, NumericEvaluationError> {
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
            .map(|initial| self.expression(initial))
            .collect::<Result<Vec<_>, _>>()?;
        let domain = self
            .view
            .domain(fold_view.domain())
            .expect("checked function loop domain resolves");
        for point_index in 0..domain.scalar_count() as usize {
            let point = function_loop_point(domain, point_index, span)?;
            self.function_fold_values.push((fold, values));
            self.domain_points.push((fold_view.domain(), point));
            let next = self.function_loop_iteration(fold_view, statements.clone());
            self.domain_points.pop();
            let (_, previous) = self
                .function_fold_values
                .pop()
                .expect("active function fold stack remains balanced");
            values = next?;
            debug_assert_eq!(values.len(), previous.len());
        }
        Ok(values)
    }

    fn function_call(
        &mut self,
        function: dae::FunctionId<'dae>,
        output: u32,
        arguments: dae::ExpressionOperands<'dae>,
        span: Span,
    ) -> Result<Vec<f64>, NumericEvaluationError> {
        if self.function_arguments.len() >= 256 {
            return Err(failure(
                NumericEvaluationErrorKind::CyclicDependency,
                "function evaluation exceeded the checked recursion limit",
                span,
            ));
        }
        let arguments = arguments
            .iter()
            .map(|argument| self.expression(argument))
            .collect::<Result<Vec<_>, _>>()?;
        let definition = self.view.function(function).ok_or_else(|| {
            failure(
                NumericEvaluationErrorKind::UnsupportedOperation,
                "function identity does not resolve",
                span,
            )
        })?;
        // MLS §12.9 external bodies are foreign code. Numeric evaluation owns
        // no runtime that can execute one, so it fails with the call's exact
        // provenance instead of substituting a plausible value.
        if let Some(external) = definition.external() {
            return Err(external_function_failure(definition.name(), external, span));
        }
        let result = definition
            .result_values()
            .rhs(output as usize)
            .ok_or_else(|| function_result_error(span))?;
        self.function_arguments.push((function, arguments));
        let value = self
            .function_statements(definition.statements())
            .and_then(|()| self.expression(result));
        self.function_arguments.pop();
        value
    }

    fn function_statements(
        &mut self,
        statements: dae::FunctionStatements<'dae>,
    ) -> Result<(), NumericEvaluationError> {
        for statement in statements {
            match statement {
                dae::FunctionStatementView::Assignment { .. } => {}
                dae::FunctionStatementView::Assertion {
                    condition,
                    message: _,
                    provenance,
                } => self.function_assertion(condition, provenance.span())?,
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
        span: Span,
    ) -> Result<(), NumericEvaluationError> {
        match self.expression(condition)?.as_slice() {
            [1.0] => Ok(()),
            [0.0] => Err(failure(
                NumericEvaluationErrorKind::AssertionFailed,
                "function assertion failed",
                span,
            )),
            _ => Err(failure(
                NumericEvaluationErrorKind::InvalidValue,
                "function assertion condition is not scalar Boolean",
                span,
            )),
        }
    }

    fn function_loop_statements(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        statements: dae::FunctionStatements<'dae>,
        span: Span,
    ) -> Result<(), NumericEvaluationError> {
        self.function_fold_values(fold, Some(statements), span)
            .map(drop)
    }

    fn function_loop_iteration(
        &mut self,
        fold: dae::FunctionFoldView<'dae>,
        statements: Option<dae::FunctionStatements<'dae>>,
    ) -> Result<Vec<Vec<f64>>, NumericEvaluationError> {
        if let Some(statements) = statements {
            self.function_statements(statements)?;
        }
        fold.update_values()
            .rhs_iter()
            .map(|update| self.expression(update))
            .collect()
    }

    /// Evaluate the constructor-proven initial value of a variable.
    pub fn initial_value(
        &mut self,
        id: dae::VariableId<'dae>,
    ) -> Result<Vec<f64>, NumericEvaluationError> {
        let variable = self
            .view
            .variable(id)
            .expect("finalized variable identity resolves");
        if variable.scalar_count() == 0 {
            return Ok(Vec::new());
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
                variable.start().or(variable.binding()).ok_or_else(|| {
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
    ) -> Result<Vec<f64>, NumericEvaluationError> {
        if variable.scalar_count() == 0 {
            return Ok(Vec::new());
        }
        if let Some(binding) = variable.binding() {
            return self.variable_expression(variable, binding);
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
            values.push(value);
        }
        Ok(values)
    }

    fn parameter_value(
        &mut self,
        id: dae::VariableId<'dae>,
    ) -> Result<Vec<f64>, NumericEvaluationError> {
        let index = id.index() as usize;
        if let Some(value) = &self.values[index] {
            return Ok(value.clone());
        }
        let variable = self
            .view
            .variable(id)
            .expect("finalized parameter identity resolves");
        if variable.scalar_count() == 0 {
            self.values[index] = Some(Vec::new());
            return Ok(Vec::new());
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
        let expression = variable.binding().or(variable.start()).ok_or_else(|| {
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
    ) -> Result<Vec<f64>, NumericEvaluationError> {
        let mut values = self.expression(expression)?;
        if values.len() == 1 && variable.scalar_count() > 1 {
            values.resize(variable.scalar_count(), values[0]);
        }
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
        self.apply_overrides(variable, &mut values)?;
        require_finite(&values, self.expression_span(expression))?;
        Ok(values)
    }

    fn apply_overrides(
        &mut self,
        variable: dae::VariableView<'dae>,
        values: &mut [f64],
    ) -> Result<(), NumericEvaluationError> {
        for (scalar, value) in values.iter_mut().enumerate() {
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
            *value = override_value;
        }
        Ok(())
    }

    fn conditional(
        &mut self,
        operands: dae::ExpressionOperands<'dae>,
        span: Span,
    ) -> Result<Vec<f64>, NumericEvaluationError> {
        for ordinal in (0..operands.len() - 1).step_by(2) {
            let condition = self.expression(
                operands
                    .get(ordinal)
                    .expect("checked conditional condition resolves"),
            )?;
            if condition.as_slice() == [1.0] {
                return self.expression(
                    operands
                        .get(ordinal + 1)
                        .expect("checked conditional value resolves"),
                );
            }
            if condition.as_slice() != [0.0] {
                return Err(failure(
                    NumericEvaluationErrorKind::InvalidValue,
                    "conditional guard is not scalar Boolean",
                    span,
                ));
            }
        }
        self.expression(
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
    ) -> Result<Vec<f64>, NumericEvaluationError> {
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
        let lhs = self.expression(lhs_id)?;
        let rhs = self.expression(rhs_id)?;
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
                binary(
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
        result_type: &dae::ValueType,
        span: Span,
    ) -> Result<Vec<f64>, NumericEvaluationError> {
        let base_node = self
            .view
            .expression(base)
            .expect("checked index base resolves");
        if !result_type.is_scalar() || subscripts.len() != base_node.value_type().dimensions().len()
        {
            return Err(failure(
                NumericEvaluationErrorKind::UnsupportedOperation,
                "numeric evaluation currently requires fully scalar array selection",
                span,
            ));
        }
        let flat = self.scalar_subscript_index(base_node, subscripts, span)?;
        self.expression(base)?
            .get(flat)
            .copied()
            .map(|value| vec![value])
            .ok_or_else(|| {
                failure(
                    NumericEvaluationErrorKind::OutOfBounds,
                    "checked array selection did not resolve",
                    span,
                )
            })
    }

    fn array_update(
        &mut self,
        base: dae::ExprId<'dae>,
        value: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        span: Span,
    ) -> Result<Vec<f64>, NumericEvaluationError> {
        let base_node = self
            .view
            .expression(base)
            .expect("checked array-update base resolves");
        let flat = self.scalar_subscript_index(base_node, subscripts, span)?;
        let updated = self.expression(value)?;
        let [value] = updated.as_slice() else {
            return Err(failure(
                NumericEvaluationErrorKind::ShapeMismatch,
                "scalar array update received a nonscalar value",
                span,
            ));
        };
        let mut result = self.expression(base)?;
        let target = result.get_mut(flat).ok_or_else(|| {
            failure(
                NumericEvaluationErrorKind::OutOfBounds,
                "checked array update did not resolve",
                span,
            )
        })?;
        *target = *value;
        Ok(result)
    }

    fn scalar_subscript_index(
        &mut self,
        base_node: dae::ExpressionView<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        span: Span,
    ) -> Result<usize, NumericEvaluationError> {
        if subscripts.len() != base_node.value_type().dimensions().len() {
            return Err(failure(
                NumericEvaluationErrorKind::UnsupportedOperation,
                "numeric evaluation currently requires fully scalar array selection",
                span,
            ));
        }
        let mut flat = 0usize;
        for (axis, extent) in base_node
            .value_type()
            .dimensions()
            .iter()
            .copied()
            .enumerate()
        {
            let dae::SubscriptView::Index {
                expression,
                provenance,
            } = subscripts
                .get(axis)
                .expect("checked index has one subscript per selected axis")
            else {
                return Err(failure(
                    NumericEvaluationErrorKind::UnsupportedOperation,
                    "numeric evaluation does not support whole or slice selection",
                    span,
                ));
            };
            let index = self.expression(expression)?;
            let [index] = index.as_slice() else {
                return Err(failure(
                    NumericEvaluationErrorKind::ShapeMismatch,
                    "array index is not scalar",
                    provenance.span(),
                ));
            };
            let rounded = index.round();
            if *index != rounded || rounded < 1.0 || rounded > f64::from(extent) {
                return Err(failure(
                    NumericEvaluationErrorKind::OutOfBounds,
                    format!("array index {index} is outside 1..={extent}"),
                    provenance.span(),
                ));
            }
            flat = flat
                .checked_mul(extent as usize)
                .and_then(|value| value.checked_add(rounded as usize - 1))
                .ok_or_else(|| {
                    failure(
                        NumericEvaluationErrorKind::Overflow,
                        "array index calculation overflowed",
                        span,
                    )
                })?;
        }
        Ok(flat)
    }

    fn builtin(
        &mut self,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'dae>,
        result_dimensions: &[u32],
        span: Span,
    ) -> Result<Vec<f64>, NumericEvaluationError> {
        if matches!(
            builtin,
            dae::PureBuiltin::PromotedCat1 | dae::PureBuiltin::PromotedCat2
        ) {
            let axis = usize::from(builtin == dae::PureBuiltin::PromotedCat2);
            return self.promoted_concatenation(arguments, axis, result_dimensions, span);
        }
        if builtin == dae::PureBuiltin::Identity {
            return Ok(identity_values(result_dimensions));
        }
        if matches!(builtin, dae::PureBuiltin::Zeros | dae::PureBuiltin::Ones) {
            let value = if builtin == dae::PureBuiltin::Ones {
                1.0
            } else {
                0.0
            };
            return self.filled_array(arguments, 0, value, span);
        }
        if builtin == dae::PureBuiltin::Fill {
            let fill =
                self.expression(arguments.get(0).expect("checked fill has a value argument"))?;
            let [fill] = fill.as_slice() else {
                unreachable!("checked fill value is scalar")
            };
            return self.filled_array(arguments, 1, *fill, span);
        }
        if builtin == dae::PureBuiltin::Linspace {
            return self.linspace(arguments);
        }
        if builtin == dae::PureBuiltin::Cross {
            return self.cross(arguments);
        }
        let first = arguments.get(0).expect("checked builtin operand");
        let mut values = self.expression(first)?;
        use dae::PureBuiltin as B;
        match builtin {
            dae::PureBuiltin::Abs => values.iter_mut().for_each(|value| *value = value.abs()),
            dae::PureBuiltin::Sign => values.iter_mut().for_each(|value| *value = value.signum()),
            dae::PureBuiltin::Sqrt => values.iter_mut().for_each(|value| *value = value.sqrt()),
            dae::PureBuiltin::Div | dae::PureBuiltin::Mod | dae::PureBuiltin::Rem => {
                let rhs = self.expression(
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
                .for_each(|value| *value = rumoca_core::modelica_integer_value(*value)),
            dae::PureBuiltin::Sin => values.iter_mut().for_each(|value| *value = value.sin()),
            dae::PureBuiltin::Cos => values.iter_mut().for_each(|value| *value = value.cos()),
            dae::PureBuiltin::Tan => values.iter_mut().for_each(|value| *value = value.tan()),
            dae::PureBuiltin::Asin => values.iter_mut().for_each(|value| *value = value.asin()),
            dae::PureBuiltin::Acos => values.iter_mut().for_each(|value| *value = value.acos()),
            dae::PureBuiltin::Atan => values.iter_mut().for_each(|value| *value = value.atan()),
            dae::PureBuiltin::Atan2 => {
                let rhs =
                    self.expression(arguments.get(1).expect("checked atan2 has two arguments"))?;
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
                values =
                    self.expression(arguments.get(1).expect("checked smooth value argument"))?;
            }
            dae::PureBuiltin::NoEvent => {}
            dae::PureBuiltin::Homotopy => {}
            dae::PureBuiltin::Vector => {}
            dae::PureBuiltin::Transpose => values = transpose_values(&values, result_dimensions),
            B::Diagonal | B::OuterProduct | B::Skew => {
                values = self.matrix_product(builtin, arguments, values)?;
            }
            dae::PureBuiltin::Sum => values = vec![values.iter().sum()],
            dae::PureBuiltin::Product => values = vec![values.iter().product()],
            dae::PureBuiltin::Min | dae::PureBuiltin::Max => {
                values = self.extremum(builtin, arguments, values, span)?;
            }
            dae::PureBuiltin::Size => values = self.size(arguments, first, span)?,
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
        Ok(values)
    }

    fn matrix_product(
        &mut self,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'dae>,
        lhs: Vec<f64>,
    ) -> Result<Vec<f64>, NumericEvaluationError> {
        match builtin {
            dae::PureBuiltin::Diagonal => Ok(diagonal_values(&lhs)),
            dae::PureBuiltin::OuterProduct => {
                let rhs = self.expression(
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
    ) -> Result<Vec<f64>, NumericEvaluationError> {
        let operands = arguments
            .iter()
            .map(|argument| {
                let dimensions = self
                    .view
                    .expression(argument)
                    .expect("checked concatenation operand resolves")
                    .value_type()
                    .dimensions()
                    .to_vec();
                self.expression(argument).map(|values| (dimensions, values))
            })
            .collect::<Result<Vec<_>, _>>()?;
        let count = result_dimensions
            .iter()
            .try_fold(1_usize, |count, extent| count.checked_mul(*extent as usize))
            .ok_or_else(|| {
                failure(
                    NumericEvaluationErrorKind::Overflow,
                    "checked concatenation scalar count overflowed",
                    span,
                )
            })?;
        Ok((0..count)
            .map(|scalar| promoted_concatenation_value(&operands, axis, result_dimensions, scalar))
            .collect())
    }

    fn filled_array(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        first_extent: usize,
        value: f64,
        span: Span,
    ) -> Result<Vec<f64>, NumericEvaluationError> {
        let mut count = 1_usize;
        for argument in arguments.iter().skip(first_extent) {
            let extent = self.expression(argument)?;
            let [extent] = extent.as_slice() else {
                return Err(failure(
                    NumericEvaluationErrorKind::ShapeMismatch,
                    "checked array-constructor extent is not scalar",
                    span,
                ));
            };
            if *extent < 0.0 || extent.fract() != 0.0 {
                return Err(failure(
                    NumericEvaluationErrorKind::InvalidValue,
                    "checked array-constructor extent is not a nonnegative integer",
                    span,
                ));
            }
            count = count.checked_mul(*extent as usize).ok_or_else(|| {
                failure(
                    NumericEvaluationErrorKind::Overflow,
                    "checked array-constructor scalar count overflowed",
                    span,
                )
            })?;
        }
        Ok(vec![value; count])
    }

    fn linspace(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
    ) -> Result<Vec<f64>, NumericEvaluationError> {
        let start = self.expression(arguments.get(0).expect("checked linspace start"))?[0];
        let stop = self.expression(arguments.get(1).expect("checked linspace stop"))?[0];
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
        let denominator = f64::from(count - 1);
        Ok((0..count)
            .map(|ordinal| start + (stop - start) * f64::from(ordinal) / denominator)
            .collect())
    }

    fn cross(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
    ) -> Result<Vec<f64>, NumericEvaluationError> {
        let lhs = self.expression(arguments.get(0).expect("checked cross lhs"))?;
        let rhs = self.expression(arguments.get(1).expect("checked cross rhs"))?;
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
        mut values: Vec<f64>,
        span: Span,
    ) -> Result<Vec<f64>, NumericEvaluationError> {
        if arguments.len() == 1 {
            let compare = if builtin == dae::PureBuiltin::Min {
                f64::min
            } else {
                f64::max
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
            let rhs = self.expression(argument)?;
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
    ) -> Result<Vec<f64>, NumericEvaluationError> {
        let dimensions = self
            .view
            .expression(first)
            .expect("checked size operand resolves")
            .value_type()
            .dimensions();
        let Some(dimension) = arguments.get(1) else {
            return Ok(dimensions.iter().map(|extent| f64::from(*extent)).collect());
        };
        let dimension = self.expression(dimension)?;
        let [dimension] = dimension.as_slice() else {
            return Err(failure(
                NumericEvaluationErrorKind::ShapeMismatch,
                "size dimension is not scalar",
                span,
            ));
        };
        if dimension.fract() != 0.0 || *dimension < 1.0 || *dimension > dimensions.len() as f64 {
            return Err(failure(
                NumericEvaluationErrorKind::OutOfBounds,
                "size dimension is outside the array rank",
                span,
            ));
        }
        Ok(vec![f64::from(dimensions[*dimension as usize - 1])])
    }

    fn expression_span(&self, expression: dae::ExprId<'dae>) -> Span {
        self.view
            .expression(expression)
            .expect("finalized expression identity resolves")
            .provenance()
            .span()
    }
}

fn function_loop_point(
    domain: dae::DomainView<'_>,
    point_index: usize,
    span: Span,
) -> Result<Vec<i64>, NumericEvaluationError> {
    domain
        .structured()
        .index_tuple_at(point_index)
        .map_err(|_| {
            failure(
                NumericEvaluationErrorKind::Overflow,
                "function loop domain projection overflowed",
                span,
            )
        })?
        .ok_or_else(|| {
            failure(
                NumericEvaluationErrorKind::OutOfBounds,
                "function loop domain point does not resolve",
                span,
            )
        })
}

/// Reorder one checked transpose result in row-major order. The result shape
/// already owns the exchanged first two extents, so swapping its first two
/// coordinates maps each result scalar back to the unique operand scalar.
fn transpose_values(values: &[f64], result_dimensions: &[u32]) -> Vec<f64> {
    let mut operand_dimensions = result_dimensions.to_vec();
    operand_dimensions.swap(0, 1);
    (0..values.len())
        .map(|scalar| {
            let mut coordinates = row_major_coordinates(result_dimensions, scalar)
                .expect("checked transpose scalar belongs to its result shape");
            coordinates.swap(0, 1);
            let operand_scalar = flatten_coordinates(&operand_dimensions, &coordinates)
                .expect("transposed coordinate belongs to its checked operand shape");
            values[operand_scalar]
        })
        .collect()
}

fn identity_values(dimensions: &[u32]) -> Vec<f64> {
    let [rows, columns] = dimensions else {
        unreachable!("checked identity result has rank two")
    };
    let count = (*rows as usize)
        .checked_mul(*columns as usize)
        .expect("checked identity scalar count fits usize");
    (0..count)
        .map(|scalar| {
            f64::from(u8::from(
                scalar / *columns as usize == scalar % *columns as usize,
            ))
        })
        .collect()
}

fn diagonal_values(values: &[f64]) -> Vec<f64> {
    let count = values
        .len()
        .checked_mul(values.len())
        .expect("checked diagonal scalar count fits usize");
    (0..count)
        .map(|scalar| {
            let row = scalar / values.len();
            let column = scalar % values.len();
            if row == column { values[row] } else { 0.0 }
        })
        .collect()
}

fn outer_product_values(lhs: &[f64], rhs: &[f64]) -> Vec<f64> {
    lhs.iter()
        .flat_map(|lhs| rhs.iter().map(move |rhs| lhs * rhs))
        .collect()
}

fn skew_values(values: &[f64]) -> Vec<f64> {
    let [x, y, z] = values else {
        unreachable!("checked skew operand has exactly three scalars")
    };
    vec![0.0, -*z, *y, *z, 0.0, -*x, -*y, *x, 0.0]
}

fn promoted_concatenation_value(
    operands: &[(Vec<u32>, Vec<f64>)],
    axis: usize,
    result_dimensions: &[u32],
    scalar: usize,
) -> f64 {
    let mut coordinates = row_major_coordinates(result_dimensions, scalar)
        .expect("checked result scalar belongs to its concatenation shape");
    let selected = coordinates[axis];
    let mut offset = 0_u32;
    for (dimensions, values) in operands {
        let extent = dimensions.get(axis).copied().unwrap_or(1);
        let end = offset
            .checked_add(extent)
            .expect("checked concatenation extent remains in the u32 domain");
        if selected < end {
            coordinates[axis] = selected - offset;
            let operand_scalar = flatten_coordinates(dimensions, &coordinates[..dimensions.len()])
                .expect("checked promoted coordinate belongs to its operand shape");
            return values[operand_scalar];
        }
        offset = end;
    }
    unreachable!("checked concatenation operands cover the result")
}

fn value_type_scalar_count<'dae>(
    view: dae::DaeView<'dae>,
    value_type: dae::ValueTypeId<'dae>,
) -> Option<usize> {
    let ty = view.value_type(value_type)?;
    if let Some(count) = ty.scalar_count() {
        return Some(count);
    }
    (0..ty.record_field_count()).try_fold(0usize, |count, ordinal| {
        let (_, field) = view.record_field(value_type, ordinal)?;
        count.checked_add(value_type_scalar_count(view, field)?)
    })
}

fn record_field_bounds<'dae>(
    view: dae::DaeView<'dae>,
    record: dae::ValueTypeId<'dae>,
    field: usize,
) -> Option<(usize, usize)> {
    let (_, selected) = view.record_field(record, field)?;
    let start = (0..field).try_fold(0usize, |offset, ordinal| {
        let (_, value_type) = view.record_field(record, ordinal)?;
        offset.checked_add(value_type_scalar_count(view, value_type)?)
    })?;
    Some((start, value_type_scalar_count(view, selected)?))
}

fn apply_quotient(
    builtin: dae::PureBuiltin,
    values: &mut [f64],
    rhs: Vec<f64>,
    span: Span,
) -> Result<(), NumericEvaluationError> {
    let function = match builtin {
        dae::PureBuiltin::Div => rumoca_core::BuiltinFunction::Div,
        dae::PureBuiltin::Mod => rumoca_core::BuiltinFunction::Mod,
        dae::PureBuiltin::Rem => rumoca_core::BuiltinFunction::Rem,
        _ => unreachable!("caller restricts quotient builtins"),
    };
    for (lhs, rhs) in values.iter_mut().zip(rhs) {
        let value = rumoca_core::apply_scalar_binary_math(function, *lhs, rhs);
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

fn literal_value(literal: &dae::DaeLiteral, span: Span) -> Result<f64, NumericEvaluationError> {
    match literal {
        dae::DaeLiteral::Real(value) => Ok(*value),
        dae::DaeLiteral::Integer(value) => Ok(*value as f64),
        dae::DaeLiteral::Enumeration(value) => Ok(*value as f64),
        dae::DaeLiteral::Boolean(value) => Ok(bool_value(*value)),
        dae::DaeLiteral::String(_) => Err(failure(
            NumericEvaluationErrorKind::UnsupportedOperation,
            "String cannot be evaluated as a numeric value",
            span,
        )),
    }
}

fn binary(operator: dae::BinaryOperator, lhs: f64, rhs: f64) -> f64 {
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

fn multiply_values(
    lhs: &[f64],
    rhs: &[f64],
    lhs_dimensions: &[u32],
    rhs_dimensions: &[u32],
    span: Span,
) -> Result<Vec<f64>, NumericEvaluationError> {
    if lhs_dimensions.is_empty() {
        return Ok(rhs.iter().map(|value| lhs[0] * value).collect());
    }
    if rhs_dimensions.is_empty() {
        return Ok(lhs.iter().map(|value| value * rhs[0]).collect());
    }
    let (rows, inner, columns) = match (lhs_dimensions, rhs_dimensions) {
        ([inner], [rhs_inner]) if inner == rhs_inner => (1usize, *inner as usize, 1usize),
        ([rows, inner], [rhs_inner]) if inner == rhs_inner => {
            (*rows as usize, *inner as usize, 1usize)
        }
        ([inner], [rhs_inner, columns]) if inner == rhs_inner => {
            (1usize, *inner as usize, *columns as usize)
        }
        ([rows, inner], [rhs_inner, columns]) if inner == rhs_inner => {
            (*rows as usize, *inner as usize, *columns as usize)
        }
        _ => {
            return Err(failure(
                NumericEvaluationErrorKind::ShapeMismatch,
                "checked multiplication shape is not computable",
                span,
            ));
        }
    };
    let mut result = Vec::with_capacity(rows * columns);
    for row in 0..rows {
        for column in 0..columns {
            let mut sum = 0.0;
            for term in 0..inner {
                let lhs_index = matrix_lhs_index(lhs_dimensions, row, inner, term);
                let rhs_index = matrix_rhs_index(rhs_dimensions, term, columns, column);
                sum += lhs[lhs_index] * rhs[rhs_index];
            }
            result.push(sum);
        }
    }
    Ok(result)
}

fn matrix_lhs_index(dimensions: &[u32], row: usize, inner: usize, term: usize) -> usize {
    if dimensions.len() == 1 {
        term
    } else {
        row * inner + term
    }
}

fn matrix_rhs_index(dimensions: &[u32], term: usize, columns: usize, column: usize) -> usize {
    if dimensions.len() == 1 {
        term
    } else {
        term * columns + column
    }
}

fn expect_same_length(lhs: &[f64], rhs: &[f64], span: Span) -> Result<(), NumericEvaluationError> {
    if lhs.len() == rhs.len() {
        Ok(())
    } else {
        Err(failure(
            NumericEvaluationErrorKind::ShapeMismatch,
            "builtin argument shape mismatch",
            span,
        ))
    }
}

fn combine_extremum(builtin: dae::PureBuiltin, lhs: &mut [f64], rhs: Vec<f64>) {
    for (lhs, rhs) in lhs.iter_mut().zip(rhs) {
        *lhs = if builtin == dae::PureBuiltin::Min {
            lhs.min(rhs)
        } else {
            lhs.max(rhs)
        };
    }
}

const fn bool_value(value: bool) -> f64 {
    if value { 1.0 } else { 0.0 }
}

fn range_values(
    start: i64,
    step: i64,
    stop: i64,
    span: Span,
) -> Result<Vec<f64>, NumericEvaluationError> {
    let count = if step > 0 && start <= stop {
        (i128::from(stop) - i128::from(start)) / i128::from(step) + 1
    } else if step < 0 && start >= stop {
        (i128::from(start) - i128::from(stop)) / -i128::from(step) + 1
    } else if step == 0 {
        return Err(failure(
            NumericEvaluationErrorKind::InvalidValue,
            "integer range step is zero",
            span,
        ));
    } else {
        0
    };
    let count = usize::try_from(count).map_err(|_| {
        failure(
            NumericEvaluationErrorKind::Overflow,
            "integer range cardinality overflowed",
            span,
        )
    })?;
    (0..count)
        .map(|ordinal| {
            let ordinal = i64::try_from(ordinal).map_err(|_| {
                failure(
                    NumericEvaluationErrorKind::Overflow,
                    "integer range ordinal overflowed",
                    span,
                )
            })?;
            start
                .checked_add(step.checked_mul(ordinal).ok_or_else(|| {
                    failure(
                        NumericEvaluationErrorKind::Overflow,
                        "integer range value overflowed",
                        span,
                    )
                })?)
                .map(|value| value as f64)
                .ok_or_else(|| {
                    failure(
                        NumericEvaluationErrorKind::Overflow,
                        "integer range value overflowed",
                        span,
                    )
                })
        })
        .collect()
}

fn row_major_coordinates(extents: &[u32], index: usize) -> Option<Vec<u32>> {
    let count = extents
        .iter()
        .try_fold(1_usize, |count, extent| count.checked_mul(*extent as usize))?;
    if index >= count {
        return None;
    }
    let mut remainder = index;
    let mut coordinates = vec![0_u32; extents.len()];
    for (axis, extent) in extents.iter().enumerate().rev() {
        if *extent == 0 {
            return None;
        }
        coordinates[axis] = u32::try_from(remainder % *extent as usize).ok()?;
        remainder /= *extent as usize;
    }
    Some(coordinates)
}

fn flatten_coordinates(extents: &[u32], coordinates: &[u32]) -> Option<usize> {
    if extents.len() != coordinates.len() {
        return None;
    }
    extents
        .iter()
        .zip(coordinates)
        .try_fold(0_usize, |flat, (extent, coordinate)| {
            if coordinate >= extent {
                return None;
            }
            flat.checked_mul(*extent as usize)?
                .checked_add(*coordinate as usize)
        })
}

fn require_finite(values: &[f64], span: Span) -> Result<(), NumericEvaluationError> {
    if values.iter().all(|value| value.is_finite()) {
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

#[cfg(test)]
mod tests {
    use rumoca_core::{
        ClockLattice, ClockRational, SourceMap, Span, StructuredIndexBinder, StructuredIndexDomain,
        VarName,
    };
    use rumoca_ir_dae::{
        BinaryOperator, CoordinateInput, Dae, DaeConstructionError, DaeLiteral, DaeProvenance,
        ExprId, ExpressionOperation, Expressions, PureBuiltin, ScalarType, ValueType,
    };

    use super::NumericEvaluator;

    #[test]
    fn integer_builtin_floors_negative_fractional_dae_values() {
        let mut source_map = SourceMap::new();
        let source = source_map.add("integer.mo", "integer(-1.8)");
        let at = DaeProvenance::source(Span::from_offsets(source, 0, 13)).unwrap();
        let dae = Dae::construct(source_map, |dae| {
            dae.expressions(|expressions| {
                let argument = expressions.at(at).literal(DaeLiteral::Real(-1.8))?;
                expressions
                    .at(at)
                    .builtin(PureBuiltin::Integer, [argument])?;
                Ok(())
            })
        })
        .unwrap();

        dae.inspect(|view| {
            let integer = view.expression_id(1).unwrap();
            assert_eq!(
                NumericEvaluator::new(view).expression(integer).unwrap(),
                [-2.0]
            );
        });
    }

    fn real_literals<'dae>(
        expressions: &mut Expressions<'_, 'dae>,
        at: DaeProvenance,
        values: impl IntoIterator<Item = i32>,
    ) -> Result<Vec<ExprId<'dae>>, DaeConstructionError> {
        values
            .into_iter()
            .map(|value| {
                expressions
                    .at(at)
                    .literal(DaeLiteral::Real(f64::from(value)))
            })
            .collect()
    }

    #[test]
    fn periodic_clock_interval_evaluates_to_its_exact_period() {
        let mut source_map = SourceMap::new();
        let source = source_map.add("interval.mo", "Clock(0.1); interval()");
        let clock_at = DaeProvenance::source(Span::from_offsets(source, 0, 10)).unwrap();
        let interval_at = DaeProvenance::source(Span::from_offsets(source, 12, 22)).unwrap();
        let dae = Dae::construct(source_map, |dae| {
            let clock = dae.clocks(|clocks| {
                clocks.periodic(
                    ClockLattice::new(ClockRational::new(1, 10).unwrap(), ClockRational::ZERO)
                        .unwrap(),
                    clock_at,
                )
            })?;
            dae.expressions(|expressions| {
                expressions
                    .at(interval_at)
                    .coordinate(CoordinateInput::ClockInterval(clock))
                    .map(|_| ())
            })
        })
        .unwrap();
        dae.inspect(|view| {
            assert_eq!(
                NumericEvaluator::new(view)
                    .expression(view.expression_id(0).unwrap())
                    .unwrap(),
                vec![0.1]
            );
        });
    }

    #[test]
    fn checked_quotients_preserve_modelica_sign_semantics() {
        let text = "div(-7, 3); mod(-7, 3); rem(-7, 3)";
        let mut source_map = SourceMap::new();
        let source = source_map.add("mod.mo", text);
        let at =
            |start, end| DaeProvenance::source(Span::from_offsets(source, start, end)).unwrap();
        let dae = Dae::construct(source_map, |dae| {
            let minus_seven = dae.expressions(|expressions| {
                expressions.at(at(4, 6)).literal(DaeLiteral::Integer(-7))
            })?;
            let three = dae.expressions(|expressions| {
                expressions.at(at(8, 9)).literal(DaeLiteral::Integer(3))
            })?;
            dae.expressions(|expressions| {
                expressions
                    .at(at(0, 10))
                    .builtin(PureBuiltin::Div, [minus_seven, three])
            })?;
            dae.expressions(|expressions| {
                expressions
                    .at(at(12, 22))
                    .builtin(PureBuiltin::Mod, [minus_seven, three])
            })?;
            dae.expressions(|expressions| {
                expressions
                    .at(at(24, text.len()))
                    .builtin(PureBuiltin::Rem, [minus_seven, three])
            })?;
            Ok(())
        })
        .unwrap();
        dae.inspect(|view| {
            let mut evaluator = NumericEvaluator::new(view);
            assert_eq!(
                evaluator
                    .expression(view.expression_id(2).unwrap())
                    .unwrap(),
                vec![-2.0]
            );
            assert_eq!(
                evaluator
                    .expression(view.expression_id(3).unwrap())
                    .unwrap(),
                vec![2.0]
            );
            assert_eq!(
                evaluator
                    .expression(view.expression_id(4).unwrap())
                    .unwrap(),
                vec![-1.0]
            );
        });
    }

    #[test]
    fn checked_zeros_evaluates_to_its_constructor_derived_shape() {
        let text = "zeros(2, 3)";
        let mut source_map = SourceMap::new();
        let source = source_map.add("zeros.mo", text);
        let at =
            |start, end| DaeProvenance::source(Span::from_offsets(source, start, end)).unwrap();
        let dae = Dae::construct(source_map, |dae| {
            let two = dae.expressions(|expressions| {
                expressions.at(at(6, 7)).literal(DaeLiteral::Integer(2))
            })?;
            let three = dae.expressions(|expressions| {
                expressions.at(at(9, 10)).literal(DaeLiteral::Integer(3))
            })?;
            dae.expressions(|expressions| {
                expressions
                    .at(at(0, text.len()))
                    .builtin(PureBuiltin::Zeros, [two, three])
            })?;
            Ok(())
        })
        .unwrap();

        dae.inspect(|view| {
            let zeros = view.expression_id(2).unwrap();
            assert!(matches!(
                view.expression(zeros).unwrap().operation(),
                ExpressionOperation::Builtin {
                    builtin: PureBuiltin::Zeros,
                    ..
                }
            ));
            assert_eq!(
                NumericEvaluator::new(view).expression(zeros).unwrap(),
                vec![0.0; 6]
            );
        });
    }

    #[test]
    fn checked_identity_derives_only_its_requested_scalar_view() {
        let text = "identity(3)";
        let mut source_map = SourceMap::new();
        let source = source_map.add("identity.mo", text);
        let at = DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
        let dae = Dae::construct(source_map, |dae| {
            let extent =
                dae.expressions(|expressions| expressions.at(at).literal(DaeLiteral::Integer(3)))?;
            dae.expressions(|expressions| {
                expressions.at(at).builtin(PureBuiltin::Identity, [extent])
            })?;
            Ok(())
        })
        .unwrap();

        dae.inspect(|view| {
            let identity = view.expression_id(1).unwrap();
            assert_eq!(
                NumericEvaluator::new(view).expression(identity).unwrap(),
                vec![1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0]
            );
        });
    }

    #[test]
    fn checked_vector_reuses_the_compact_operand_row_major_values() {
        let text = "vector([{{1.0},{2.0},{3.0}}])";
        let mut source_map = SourceMap::new();
        let source = source_map.add("vector.mo", text);
        let at = DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
        let dae = Dae::construct(source_map, |dae| {
            dae.expressions(|expressions| {
                let values = [1.0, 2.0, 3.0]
                    .into_iter()
                    .map(|value| expressions.at(at).literal(DaeLiteral::Real(value)))
                    .collect::<Result<Vec<_>, _>>()?;
                let columns = values
                    .into_iter()
                    .map(|value| expressions.at(at).array([value]))
                    .collect::<Result<Vec<_>, _>>()?;
                let row = expressions.at(at).array(columns)?;
                let tensor = expressions.at(at).array([row])?;
                expressions.at(at).builtin(PureBuiltin::Vector, [tensor])?;
                Ok(())
            })
        })
        .unwrap();

        dae.inspect(|view| {
            let vector = view.expression_id(8).unwrap();
            let node = view.expression(vector).unwrap();
            assert_eq!(node.value_type().dimensions(), &[3]);
            assert_eq!(
                NumericEvaluator::new(view).expression(vector).unwrap(),
                [1.0, 2.0, 3.0]
            );
        });
    }

    #[test]
    fn checked_transpose_permutes_nonsquare_and_rank_three_row_major_values() {
        let text = "transpose([1,2,3;4,5,6]); transpose(tensor)";
        let mut source_map = SourceMap::new();
        let source = source_map.add("transpose.mo", text);
        let at = DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
        let dae = Dae::construct(source_map, |dae| {
            dae.expressions(|expressions| {
                let matrix_values = real_literals(expressions, at, 1..=6)?;
                let matrix_rows = matrix_values
                    .chunks_exact(3)
                    .map(|row| expressions.at(at).array(row.iter().copied()))
                    .collect::<Result<Vec<_>, _>>()?;
                let matrix = expressions.at(at).array(matrix_rows)?;
                expressions
                    .at(at)
                    .builtin(PureBuiltin::Transpose, [matrix])?;

                let tensor_values = real_literals(expressions, at, 1..=12)?;
                let vectors = tensor_values
                    .chunks_exact(2)
                    .map(|values| expressions.at(at).array(values.iter().copied()))
                    .collect::<Result<Vec<_>, _>>()?;
                let matrices = vectors
                    .chunks_exact(3)
                    .map(|rows| expressions.at(at).array(rows.iter().copied()))
                    .collect::<Result<Vec<_>, _>>()?;
                let tensor = expressions.at(at).array(matrices)?;
                expressions
                    .at(at)
                    .builtin(PureBuiltin::Transpose, [tensor])?;
                Ok(())
            })
        })
        .unwrap();

        dae.inspect(|view| {
            let transposes = (0..view.expression_count())
                .filter_map(|index| {
                    let id = view.expression_id(index).unwrap();
                    matches!(
                        view.expression(id).unwrap().operation(),
                        ExpressionOperation::Builtin {
                            builtin: PureBuiltin::Transpose,
                            ..
                        }
                    )
                    .then_some(id)
                })
                .collect::<Vec<_>>();
            let mut evaluator = NumericEvaluator::new(view);
            assert_eq!(
                evaluator.expression(transposes[0]).unwrap(),
                [1.0, 4.0, 2.0, 5.0, 3.0, 6.0]
            );
            assert_eq!(
                evaluator.expression(transposes[1]).unwrap(),
                [
                    1.0, 2.0, 7.0, 8.0, 3.0, 4.0, 9.0, 10.0, 5.0, 6.0, 11.0, 12.0
                ]
            );
        });
    }

    #[test]
    fn checked_diagonal_and_outer_product_evaluate_compact_operands_row_major() {
        let text = "diagonal({2.0,3.0}); outerProduct({1.0,2.0},{4.0,5.0})";
        let mut source_map = SourceMap::new();
        let source = source_map.add("matrix_products.mo", text);
        let at = DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
        let dae = Dae::construct(source_map, |dae| {
            dae.expressions(|expressions| {
                let diagonal_values = [2.0, 3.0]
                    .into_iter()
                    .map(|value| expressions.at(at).literal(DaeLiteral::Real(value)))
                    .collect::<Result<Vec<_>, _>>()?;
                let diagonal_values = expressions.at(at).array(diagonal_values)?;
                expressions
                    .at(at)
                    .builtin(PureBuiltin::Diagonal, [diagonal_values])?;
                let lhs = [1.0, 2.0]
                    .into_iter()
                    .map(|value| expressions.at(at).literal(DaeLiteral::Real(value)))
                    .collect::<Result<Vec<_>, _>>()?;
                let lhs = expressions.at(at).array(lhs)?;
                let rhs = [4.0, 5.0]
                    .into_iter()
                    .map(|value| expressions.at(at).literal(DaeLiteral::Real(value)))
                    .collect::<Result<Vec<_>, _>>()?;
                let rhs = expressions.at(at).array(rhs)?;
                expressions
                    .at(at)
                    .builtin(PureBuiltin::OuterProduct, [lhs, rhs])?;
                Ok(())
            })
        })
        .unwrap();

        dae.inspect(|view| {
            let builtins = (0..view.expression_count())
                .filter_map(|index| view.expression_id(index))
                .filter(|id| {
                    matches!(
                        view.expression(*id).unwrap().operation(),
                        ExpressionOperation::Builtin {
                            builtin: PureBuiltin::Diagonal | PureBuiltin::OuterProduct,
                            ..
                        }
                    )
                })
                .collect::<Vec<_>>();
            let mut evaluator = NumericEvaluator::new(view);
            assert_eq!(
                evaluator.expression(builtins[0]).unwrap(),
                [2.0, 0.0, 0.0, 3.0]
            );
            assert_eq!(
                evaluator.expression(builtins[1]).unwrap(),
                [4.0, 5.0, 8.0, 10.0]
            );
        });
    }

    #[test]
    fn checked_skew_evaluates_one_compact_real_three_vector_row_major() {
        let text = "skew({1.0,2.0,3.0})";
        let mut source_map = SourceMap::new();
        let source = source_map.add("skew.mo", text);
        let at = DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
        let dae = Dae::construct(source_map, |dae| {
            dae.expressions(|expressions| {
                let values = [1.0, 2.0, 3.0]
                    .into_iter()
                    .map(|value| expressions.at(at).literal(DaeLiteral::Real(value)))
                    .collect::<Result<Vec<_>, _>>()?;
                let vector = expressions.at(at).array(values)?;
                expressions.at(at).builtin(PureBuiltin::Skew, [vector])?;
                Ok(())
            })
        })
        .unwrap();

        dae.inspect(|view| {
            let skew = view.expression_id(4).unwrap();
            assert_eq!(
                NumericEvaluator::new(view).expression(skew).unwrap(),
                [0.0, -3.0, 2.0, 3.0, 0.0, -1.0, -2.0, 1.0, 0.0]
            );
        });
    }

    #[test]
    fn promoted_concatenation_evaluates_in_result_row_major_order() {
        let text = "[a, b]";
        let mut source_map = SourceMap::new();
        let source = source_map.add("cat.mo", text);
        let at = DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
        let dae = Dae::construct(source_map, |dae| {
            dae.expressions(|expressions| {
                let one = expressions.at(at).literal(DaeLiteral::Real(1.0))?;
                let two = expressions.at(at).literal(DaeLiteral::Real(2.0))?;
                let three = expressions.at(at).literal(DaeLiteral::Real(3.0))?;
                let four = expressions.at(at).literal(DaeLiteral::Real(4.0))?;
                let a = expressions.at(at).array([one, two])?;
                let b = expressions.at(at).array([three, four])?;
                expressions
                    .at(at)
                    .builtin(PureBuiltin::PromotedCat2, [a, b])?;
                Ok(())
            })
        })
        .unwrap();

        dae.inspect(|view| {
            let concatenation = view.expression_id(6).unwrap();
            assert_eq!(
                NumericEvaluator::new(view)
                    .expression(concatenation)
                    .unwrap(),
                vec![1.0, 3.0, 2.0, 4.0]
            );
        });
    }

    #[test]
    fn checked_ones_and_fill_evaluate_without_materialized_dae_arrays() {
        let text = "ones(2); fill(0.5, 3)";
        let mut source_map = SourceMap::new();
        let source = source_map.add("constructors.mo", text);
        let at = |needle: &str, occurrence: usize| {
            let start = text.match_indices(needle).nth(occurrence).unwrap().0;
            DaeProvenance::source(Span::from_offsets(source, start, start + needle.len())).unwrap()
        };
        let dae = Dae::construct(source_map, |dae| {
            let two = dae.expressions(|expressions| {
                expressions.at(at("2", 0)).literal(DaeLiteral::Integer(2))
            })?;
            dae.expressions(|expressions| {
                expressions
                    .at(at("ones(2)", 0))
                    .builtin(PureBuiltin::Ones, [two])
            })?;
            let value = dae.expressions(|expressions| {
                expressions.at(at("0.5", 0)).literal(DaeLiteral::Real(0.5))
            })?;
            let three = dae.expressions(|expressions| {
                expressions.at(at("3", 0)).literal(DaeLiteral::Integer(3))
            })?;
            dae.expressions(|expressions| {
                expressions
                    .at(at("fill(0.5, 3)", 0))
                    .builtin(PureBuiltin::Fill, [value, three])
            })?;
            Ok(())
        })
        .unwrap();

        dae.inspect(|view| {
            let mut evaluator = NumericEvaluator::new(view);
            assert_eq!(
                evaluator
                    .expression(view.expression_id(1).unwrap())
                    .unwrap(),
                vec![1.0, 1.0]
            );
            assert_eq!(
                evaluator
                    .expression(view.expression_id(4).unwrap())
                    .unwrap(),
                vec![0.5, 0.5, 0.5]
            );
        });
    }

    #[test]
    fn checked_linspace_and_cross_evaluate_their_vector_semantics() {
        let text = "linspace(0.0, 2.0, 3); cross({1.0,0.0,0.0},{0.0,1.0,0.0})";
        let mut source_map = SourceMap::new();
        let source = source_map.add("vectors.mo", text);
        let at = |needle: &str, occurrence: usize| {
            let start = text.match_indices(needle).nth(occurrence).unwrap().0;
            DaeProvenance::source(Span::from_offsets(source, start, start + needle.len())).unwrap()
        };
        let dae = Dae::construct(source_map, |dae| {
            dae.expressions(|expressions| {
                let start = expressions
                    .at(at("0.0", 0))
                    .literal(DaeLiteral::Real(0.0))?;
                let stop = expressions
                    .at(at("2.0", 0))
                    .literal(DaeLiteral::Real(2.0))?;
                let count = expressions.at(at("3", 0)).literal(DaeLiteral::Integer(3))?;
                expressions
                    .at(at("linspace(0.0, 2.0, 3)", 0))
                    .builtin(PureBuiltin::Linspace, [start, stop, count])?;
                let lhs_values = [
                    expressions
                        .at(at("1.0", 0))
                        .literal(DaeLiteral::Real(1.0))?,
                    expressions
                        .at(at("0.0", 1))
                        .literal(DaeLiteral::Real(0.0))?,
                    expressions
                        .at(at("0.0", 2))
                        .literal(DaeLiteral::Real(0.0))?,
                ];
                let lhs = expressions.at(at("{1.0,0.0,0.0}", 0)).array(lhs_values)?;
                let rhs_values = [
                    expressions
                        .at(at("0.0", 3))
                        .literal(DaeLiteral::Real(0.0))?,
                    expressions
                        .at(at("1.0", 1))
                        .literal(DaeLiteral::Real(1.0))?,
                    expressions
                        .at(at("0.0", 4))
                        .literal(DaeLiteral::Real(0.0))?,
                ];
                let rhs = expressions.at(at("{0.0,1.0,0.0}", 0)).array(rhs_values)?;
                expressions
                    .at(at("cross({1.0,0.0,0.0},{0.0,1.0,0.0})", 0))
                    .builtin(PureBuiltin::Cross, [lhs, rhs])?;
                Ok(())
            })
        })
        .unwrap();

        dae.inspect(|view| {
            let mut evaluator = NumericEvaluator::new(view);
            assert_eq!(
                evaluator
                    .expression(view.expression_id(3).unwrap())
                    .unwrap(),
                vec![0.0, 1.0, 2.0]
            );
            assert_eq!(
                evaluator
                    .expression(view.expression_id(12).unwrap())
                    .unwrap(),
                vec![0.0, 0.0, 1.0]
            );
        });
    }

    #[test]
    fn zero_cardinality_variables_have_the_unique_empty_value() {
        let text = "parameter Real p[0]; Real z[0];";
        let mut source_map = SourceMap::new();
        let source = source_map.add("empty.mo", text);
        let at = |needle: &str| {
            let start = text.find(needle).unwrap();
            DaeProvenance::source(Span::from_offsets(source, start, start + needle.len())).unwrap()
        };
        let dae = Dae::construct(source_map, |dae| {
            let empty = dae.types(|types| {
                types.derived(ValueType::array(ScalarType::Real, [0]), at("Real p[0]"))
            })?;
            dae.variables(|variables| {
                variables.parameter(
                    VarName::new("p"),
                    empty,
                    at("parameter Real p[0]"),
                    rumoca_ir_dae::VariableAttributes::default(),
                )?;
                variables.algebraic(
                    VarName::new("z"),
                    empty,
                    at("Real z[0]"),
                    rumoca_ir_dae::VariableAttributes::default(),
                )?;
                Ok(())
            })
        })
        .unwrap();

        dae.inspect(|view| {
            let mut evaluator = NumericEvaluator::new(view);
            for index in 0..2 {
                let variable = view.variable_id(index).unwrap();
                assert_eq!(
                    evaluator.initial_value(variable).unwrap(),
                    Vec::<f64>::new()
                );
            }
        });
    }

    #[test]
    fn runtime_inputs_require_an_override_or_use_their_checked_default() {
        let text = "input Real defaulted = 2.5; input Real supplied;";
        let mut source_map = SourceMap::new();
        let source = source_map.add("inputs.mo", text);
        let at = |needle: &str| {
            let start = text.find(needle).unwrap();
            DaeProvenance::source(Span::from_offsets(source, start, start + needle.len())).unwrap()
        };
        let dae = Dae::construct(source_map, |dae| {
            let real = dae.types(|types| {
                types.derived(ValueType::scalar(ScalarType::Real), at("Real defaulted"))
            })?;
            let default = dae.expressions(|expressions| {
                expressions.at(at("2.5")).literal(DaeLiteral::Real(2.5))
            })?;
            dae.variables(|variables| {
                variables.input(
                    VarName::new("defaulted"),
                    real,
                    rumoca_ir_dae::InputVariability::Continuous,
                    at("input Real defaulted"),
                    rumoca_ir_dae::VariableAttributes {
                        binding: Some(default),
                        causality: rumoca_ir_dae::VariableCausality::Input,
                        ..Default::default()
                    },
                )?;
                variables.input(
                    VarName::new("supplied"),
                    real,
                    rumoca_ir_dae::InputVariability::Continuous,
                    at("input Real supplied"),
                    rumoca_ir_dae::VariableAttributes {
                        causality: rumoca_ir_dae::VariableCausality::Input,
                        ..Default::default()
                    },
                )?;
                Ok(())
            })
        })
        .unwrap();

        dae.inspect(|view| {
            let defaulted = view.variable_id(0).unwrap();
            let supplied = view.variable_id(1).unwrap();
            let mut without_provider = NumericEvaluator::new(view);
            assert_eq!(
                without_provider.initial_value(defaulted).unwrap(),
                vec![2.5]
            );
            assert_eq!(
                without_provider.initial_value(supplied).unwrap_err().kind(),
                super::NumericEvaluationErrorKind::MissingValue
            );

            let mut with_provider = NumericEvaluator::with_overrides(view, |variable, _| {
                match variable.name().as_str() {
                    "defaulted" => Some(3.5),
                    "supplied" => Some(4.5),
                    _ => None,
                }
            });
            assert_eq!(with_provider.initial_value(defaulted).unwrap(), vec![3.5]);
            assert_eq!(with_provider.initial_value(supplied).unwrap(), vec![4.5]);
        });
    }

    #[test]
    fn nested_comprehensions_evaluate_with_lexically_scoped_binders() {
        let text = "{{i + j for j in 1:3} for i in 1:2}";
        let mut source_map = SourceMap::new();
        let source = source_map.add("nested.mo", text);
        let at = |needle: &str, occurrence: usize| {
            let start = text.match_indices(needle).nth(occurrence).unwrap().0;
            DaeProvenance::source(Span::from_offsets(source, start, start + needle.len())).unwrap()
        };
        let singleton_domain = |name: &str, upper| StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                id: 0,
                display_name: name.to_string(),
                lower: 1,
                upper,
                step: 1,
            }],
        };
        let dae = Dae::construct(source_map, |dae| {
            let outer =
                dae.domains(|domains| domains.structured(singleton_domain("i", 2), at("1:2", 0)))?;
            let i = dae.domains(|domains| domains.binder(outer, 0, at("i", 0)))?;
            let inner = dae.domains(|domains| {
                domains.nested_in_scope([i], singleton_domain("j", 3), at("1:3", 0))
            })?;
            let j = dae.domains(|domains| domains.binder(inner, 0, at("j", 0)))?;
            dae.expressions(|expressions| {
                let i = expressions.at(at("i", 0)).binder(i)?;
                let j = expressions.at(at("j", 0)).binder(j)?;
                let sum = expressions
                    .at(at("i + j", 0))
                    .binary(BinaryOperator::Add, i, j)?;
                let inner = expressions
                    .at(at("{i + j for j in 1:3}", 0))
                    .comprehension(inner, sum)?;
                expressions.at(at(text, 0)).comprehension(outer, inner)?;
                Ok(())
            })
        })
        .unwrap();

        dae.inspect(|view| {
            let nested = view.expression_id(4).unwrap();
            assert_eq!(
                NumericEvaluator::new(view).expression(nested).unwrap(),
                vec![2.0, 3.0, 4.0, 3.0, 4.0, 5.0]
            );
        });
    }

    #[test]
    fn function_assertions_execute_for_each_call_without_numeric_messages() {
        let text = "function checked input Boolean ok; output Real y; algorithm \
                    assert(ok, \"message\"); y := 1.0; end checked; \
                    checked(true); checked(false)";
        let mut source_map = SourceMap::new();
        let source = source_map.add("checked.mo", text);
        let at = |needle: &str, occurrence: usize| {
            let start = text.match_indices(needle).nth(occurrence).unwrap().0;
            DaeProvenance::source(Span::from_offsets(source, start, start + needle.len())).unwrap()
        };
        let assertion_at = at("assert(ok, \"message\")", 0);
        let dae = Dae::construct(source_map, |dae| {
            let boolean = dae.types(|types| {
                types.derived(ValueType::scalar(ScalarType::Boolean), at("Boolean", 0))
            })?;
            let real = dae
                .types(|types| types.derived(ValueType::scalar(ScalarType::Real), at("Real", 0)))?;
            let function = construct_asserting_function(dae, boolean, real, at)?;
            let true_value = dae.expressions(|expressions| {
                expressions
                    .at(at("true", 0))
                    .literal(DaeLiteral::Boolean(true))
            })?;
            dae.expressions(|expressions| {
                expressions
                    .at(at("checked(true)", 0))
                    .call(function, 0, [true_value])
            })?;
            let false_value = dae.expressions(|expressions| {
                expressions
                    .at(at("false", 0))
                    .literal(DaeLiteral::Boolean(false))
            })?;
            dae.expressions(|expressions| {
                expressions
                    .at(at("checked(false)", 0))
                    .call(function, 0, [false_value])
            })?;
            Ok(())
        })
        .unwrap();

        dae.inspect(|view| {
            let calls = (0..view.expression_count())
                .filter_map(|index| view.expression_id(index))
                .filter(|id| {
                    matches!(
                        view.expression(*id).unwrap().operation(),
                        ExpressionOperation::Call { .. }
                    )
                })
                .collect::<Vec<_>>();
            assert_eq!(calls.len(), 2);
            let mut evaluator = NumericEvaluator::new(view);
            assert_eq!(evaluator.expression(calls[0]).unwrap(), vec![1.0]);
            let error = evaluator.expression(calls[1]).unwrap_err();
            assert_eq!(
                error.kind(),
                super::NumericEvaluationErrorKind::AssertionFailed
            );
            assert_eq!(error.span(), assertion_at.span());
        });
    }

    fn construct_asserting_function<'dae>(
        dae: &mut rumoca_ir_dae::DaeConstruction<'dae>,
        boolean: rumoca_ir_dae::ValueTypeId<'dae>,
        real: rumoca_ir_dae::ValueTypeId<'dae>,
        at: impl Copy + Fn(&str, usize) -> DaeProvenance,
    ) -> Result<rumoca_ir_dae::FunctionId<'dae>, rumoca_ir_dae::DaeConstructionError> {
        let signature = rumoca_ir_dae::FunctionSignature::new(
            VarName::new("checked"),
            [boolean],
            [real],
            at("function checked", 0),
        );
        dae.function(signature, |dae, reservation| {
            let input = dae.functions(|functions| {
                functions.parameter(&reservation, VarName::new("ok"), 0, at("Boolean ok", 0))
            })?;
            let output = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("y"), 0, at("Real y", 0))
            })?;
            let mut body =
                dae.functions(|functions| functions.begin(reservation, at("function checked", 0)))?;
            let condition = dae
                .expressions(|expressions| expressions.at(at("ok", 1)).function_parameter(input))?;
            let message = dae.expressions(|expressions| {
                expressions
                    .at(at("\"message\"", 0))
                    .literal(DaeLiteral::String("message".to_string()))
            })?;
            dae.functions(|functions| {
                functions.assertion(
                    &mut body,
                    condition,
                    message,
                    at("assert(ok, \"message\")", 0),
                )
            })?;
            let one = dae.expressions(|expressions| {
                expressions.at(at("1.0", 0)).literal(DaeLiteral::Real(1.0))
            })?;
            dae.functions(|functions| functions.assign(&mut body, output, one, at("y := 1.0", 0)))?;
            dae.functions(|functions| functions.define(body, at("function checked", 0)))
        })
        .map(|(function, ())| function)
    }

    #[test]
    fn checked_function_fold_evaluates_its_compact_transition() {
        let text = "function sum3 output Integer y; algorithm y := 0; \
                    for k in 1:3 loop y := y + k; end for; end sum3; sum3()";
        let mut source_map = SourceMap::new();
        let source = source_map.add("sum3.mo", text);
        let at = |needle: &str, occurrence: usize| {
            let start = text.match_indices(needle).nth(occurrence).unwrap().0;
            DaeProvenance::source(Span::from_offsets(source, start, start + needle.len())).unwrap()
        };
        let dae = Dae::construct(source_map, |dae| {
            let integer = dae.types(|types| {
                types.derived(ValueType::scalar(ScalarType::Integer), at("Integer", 0))
            })?;
            let function = construct_sum3_function(dae, integer, at)?;
            dae.expressions(|expressions| expressions.at(at("sum3()", 0)).call(function, 0, []))?;
            Ok(())
        })
        .unwrap();

        dae.inspect(|view| {
            let call = view.expression_id(view.expression_count() - 1).unwrap();
            assert_eq!(
                NumericEvaluator::new(view).expression(call).unwrap(),
                vec![6.0]
            );
        });
    }

    fn construct_sum3_function<'dae>(
        dae: &mut rumoca_ir_dae::DaeConstruction<'dae>,
        integer: rumoca_ir_dae::ValueTypeId<'dae>,
        at: impl Copy + Fn(&str, usize) -> DaeProvenance,
    ) -> Result<rumoca_ir_dae::FunctionId<'dae>, rumoca_ir_dae::DaeConstructionError> {
        let signature = rumoca_ir_dae::FunctionSignature::new(
            VarName::new("sum3"),
            [],
            [integer],
            at("function sum3", 0),
        );
        dae.function(signature, |dae, reservation| {
            let output = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("y"), 0, at("Integer y", 0))
            })?;
            let mut body =
                dae.functions(|functions| functions.begin(reservation, at("function sum3", 0)))?;
            let zero = dae.expressions(|expressions| {
                expressions.at(at("0", 0)).literal(DaeLiteral::Integer(0))
            })?;
            dae.functions(|functions| functions.assign(&mut body, output, zero, at("y := 0", 0)))?;
            let domain = dae.domains(|domains| {
                domains.structured(
                    StructuredIndexDomain {
                        binders: vec![StructuredIndexBinder {
                            id: 0,
                            display_name: "k".to_string(),
                            lower: 1,
                            upper: 3,
                            step: 1,
                        }],
                    },
                    at("1:3", 0),
                )
            })?;
            let binder = dae.domains(|domains| domains.binder(domain, 0, at("k", 0)))?;
            let mut loop_body = dae.functions(|functions| {
                functions.begin_loop(
                    body,
                    domain,
                    [output],
                    at("for k in 1:3 loop y := y + k; end for", 0),
                )
            })?;
            let current =
                dae.functions(|functions| functions.read(loop_body.body(), output, at("y", 3)))?;
            let k = dae.expressions(|expressions| expressions.at(at("k", 1)).binder(binder))?;
            let update = dae.expressions(|expressions| {
                expressions
                    .at(at("y + k", 0))
                    .binary(BinaryOperator::Add, current, k)
            })?;
            dae.functions(|functions| {
                functions.assign_loop(&mut loop_body, output, update, at("y := y + k", 0))
            })?;
            let body = dae.functions(|functions| {
                functions.finish_loop(loop_body, at("for k in 1:3 loop y := y + k; end for", 0))
            })?;
            dae.functions(|functions| functions.define(body, at("function sum3", 0)))
        })
        .map(|(function, ())| function)
    }
}
