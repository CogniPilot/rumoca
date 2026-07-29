use rumoca_core::Span;
use rumoca_ir_dae as dae;

/// Stable categories for failures while evaluating a checked DAE expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumericEvaluationErrorKind {
    CyclicDependency,
    MissingValue,
    NonStaticCoordinate,
    UnsupportedOperation,
    ShapeMismatch,
    InvalidValue,
    OutOfBounds,
    Overflow,
    InvalidOverride,
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
/// bindings. Every other coordinate is rejected: this evaluator never invents
/// a runtime value. The optional override function is applied at the variable
/// boundary, so dependents observe the same overridden parameter values as the
/// runtime layout.
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
            dae::ExpressionOperation::Range { start, step, stop } => {
                range_values(start, step, stop, span)?
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                self.index(base, subscripts, node.value_type(), span)?
            }
            dae::ExpressionOperation::ArrayUpdate {
                base,
                value,
                subscripts,
            } => self.array_update(base, value, subscripts, span)?,
            dae::ExpressionOperation::Builtin { builtin, arguments } => {
                self.builtin(builtin, arguments, span)?
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
                self.expression(definition)?
            }
            dae::ExpressionOperation::FunctionFoldParameter { fold, carried } => self
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
            dae::ExpressionOperation::FunctionFoldOutput { fold, carried } => {
                self.function_fold(fold, carried, span)?
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
        let fold_view = self.view.function_fold(fold).ok_or_else(|| {
            failure(
                NumericEvaluationErrorKind::UnsupportedOperation,
                "function loop identity does not resolve",
                span,
            )
        })?;
        let mut values = fold_view
            .initial_values()
            .iter()
            .map(|initial| self.expression(initial))
            .collect::<Result<Vec<_>, _>>()?;
        let domain = self
            .view
            .domain(fold_view.domain())
            .expect("checked function loop domain resolves");
        for point_index in 0..domain.scalar_count() as usize {
            let point = domain
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
                })?;
            self.function_fold_values.push((fold, values));
            self.domain_points.push((fold_view.domain(), point));
            let next = fold_view
                .update_values()
                .iter()
                .map(|update| self.expression(update))
                .collect::<Result<Vec<_>, _>>();
            self.domain_points.pop();
            let (_, previous) = self
                .function_fold_values
                .pop()
                .expect("active function fold stack remains balanced");
            values = next?;
            debug_assert_eq!(values.len(), previous.len());
        }
        values
            .get(carried as usize)
            .cloned()
            .ok_or_else(|| function_ordinal_error(span))
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
        let result = definition
            .result_values()
            .get(output as usize)
            .ok_or_else(|| function_result_error(span))?;
        self.function_arguments.push((function, arguments));
        let value = self.expression(result);
        self.function_arguments.pop();
        value
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
        if matches!(
            variable.role(),
            dae::VariableRole::Parameter | dae::VariableRole::Constant
        ) {
            return self.parameter_value(id);
        }
        let expression = match variable.role() {
            dae::VariableRole::Input => {
                return Err(failure(
                    NumericEvaluationErrorKind::MissingValue,
                    format!(
                        "input `{}` has no runtime input provider; evaluation cannot invent its value",
                        variable.name()
                    ),
                    variable.declaration().span(),
                ));
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
            if !variable.is_tunable() {
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
        span: Span,
    ) -> Result<Vec<f64>, NumericEvaluationError> {
        let first = arguments
            .get(0)
            .expect("checked builtin has its required operand");
        let mut values = self.expression(first)?;
        match builtin {
            dae::PureBuiltin::Abs => values.iter_mut().for_each(|value| *value = value.abs()),
            dae::PureBuiltin::Sign => values.iter_mut().for_each(|value| *value = value.signum()),
            dae::PureBuiltin::Sqrt => values.iter_mut().for_each(|value| *value = value.sqrt()),
            dae::PureBuiltin::Mod => {
                let rhs =
                    self.expression(arguments.get(1).expect("checked mod has two arguments"))?;
                apply_mod(&mut values, rhs, span)?;
            }
            dae::PureBuiltin::Floor => values.iter_mut().for_each(|value| *value = value.floor()),
            dae::PureBuiltin::Ceil => values.iter_mut().for_each(|value| *value = value.ceil()),
            dae::PureBuiltin::Integer => values.iter_mut().for_each(|value| *value = value.trunc()),
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
            dae::PureBuiltin::Sum => values = vec![values.iter().sum()],
            dae::PureBuiltin::Product => values = vec![values.iter().product()],
            dae::PureBuiltin::Min | dae::PureBuiltin::Max => {
                values = self.extremum(builtin, arguments, values, span)?;
            }
            dae::PureBuiltin::Size => values = self.size(arguments, first, span)?,
            dae::PureBuiltin::Zeros => values = self.zeros(arguments, span)?,
        }
        Ok(values)
    }

    fn zeros(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        span: Span,
    ) -> Result<Vec<f64>, NumericEvaluationError> {
        let mut count = 1_usize;
        for argument in arguments.iter() {
            let extent = self.expression(argument)?;
            let [extent] = extent.as_slice() else {
                return Err(failure(
                    NumericEvaluationErrorKind::ShapeMismatch,
                    "checked zeros extent is not scalar",
                    span,
                ));
            };
            if *extent < 0.0 || extent.fract() != 0.0 {
                return Err(failure(
                    NumericEvaluationErrorKind::InvalidValue,
                    "checked zeros extent is not a nonnegative integer",
                    span,
                ));
            }
            count = count.checked_mul(*extent as usize).ok_or_else(|| {
                failure(
                    NumericEvaluationErrorKind::Overflow,
                    "checked zeros scalar count overflowed",
                    span,
                )
            })?;
        }
        Ok(vec![0.0; count])
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

fn apply_mod(values: &mut [f64], rhs: Vec<f64>, span: Span) -> Result<(), NumericEvaluationError> {
    for (lhs, rhs) in values.iter_mut().zip(rhs) {
        let value =
            rumoca_core::apply_scalar_binary_math(rumoca_core::BuiltinFunction::Mod, *lhs, rhs);
        *lhs = value.ok_or_else(|| {
            failure(
                NumericEvaluationErrorKind::InvalidValue,
                "mod divisor must be nonzero",
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
    use rumoca_core::{SourceMap, Span, StructuredIndexBinder, StructuredIndexDomain, VarName};
    use rumoca_ir_dae::{
        BinaryOperator, Dae, DaeLiteral, DaeProvenance, ExpressionOperation, PureBuiltin,
        ScalarType, ValueType,
    };

    use super::NumericEvaluator;

    #[test]
    fn checked_mod_preserves_modelica_sign_semantics_and_rejects_zero_divisors() {
        let text = "mod(-7, 3); mod(-7, 0)";
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
                    .builtin(PureBuiltin::Mod, [minus_seven, three])
            })?;
            let zero = dae.expressions(|expressions| {
                expressions.at(at(21, 22)).literal(DaeLiteral::Integer(0))
            })?;
            dae.expressions(|expressions| {
                expressions
                    .at(at(12, text.len()))
                    .builtin(PureBuiltin::Mod, [minus_seven, zero])
            })?;
            Ok(())
        })
        .unwrap();
        dae.inspect(|view| {
            let valid = view.expression_id(2).unwrap();
            assert_eq!(
                NumericEvaluator::new(view).expression(valid).unwrap(),
                vec![2.0]
            );
            let division_by_zero = view.expression_id(4).unwrap();
            let error = NumericEvaluator::new(view)
                .expression(division_by_zero)
                .unwrap_err();
            assert_eq!(
                error.kind(),
                super::NumericEvaluationErrorKind::InvalidValue
            );
            assert_eq!(error.span(), at(12, text.len()).span());
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
            let (function, reservation) = dae.functions(|functions| {
                functions.reserve_recursive(
                    VarName::new("sum3"),
                    [],
                    [integer],
                    at("function sum3", 0),
                )
            })?;
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
                    &body,
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
            dae.functions(|functions| {
                functions.finish_loop(
                    &mut body,
                    loop_body,
                    at("for k in 1:3 loop y := y + k; end for", 0),
                )
            })?;
            dae.functions(|functions| functions.define(body, at("function sum3", 0)))?;
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
}
