use super::*;

#[derive(Clone, Copy)]
struct ExpressionValidator<'a> {
    roles: &'a HashMap<VarName, PlannedRole>,
    states: &'a HashSet<VarName>,
    binders: &'a HashSet<VarName>,
    record_array_fields: Option<&'a RecordArrayFieldPlans>,
    /// Translation-time values this scope proves, when it is a value-proven
    /// function specialization.
    ///
    /// `None` is the model scope, where no specialization has settled a
    /// coordinate. Only the compact-range rule of MLS §10.4.1 reads this, and
    /// only to decide whether a non-literal bound is nevertheless settled at
    /// translation time.
    values: Option<&'a ShapeEnvironment>,
}

pub(super) fn validate_expression(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
) -> Result<(), ToDaeError> {
    let binders = HashSet::new();
    ExpressionValidator {
        roles,
        states,
        binders: &binders,
        record_array_fields: None,
        values: None,
    }
    .validate(expression)
}

/// Validate an expression inside a value-proven function specialization.
///
/// The specialization's environment is what lets MLS §10.4.1's compact-range
/// rule accept `1:integer(m/2)`: the bound is not a literal token, but this
/// scope settles it at translation time.
pub(super) fn validate_specialized_expression(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
    values: &ShapeEnvironment,
) -> Result<(), ToDaeError> {
    let binders = HashSet::new();
    let states = HashSet::new();
    ExpressionValidator {
        roles,
        states: &states,
        binders: &binders,
        record_array_fields: None,
        values: Some(values),
    }
    .validate(expression)
}

/// Validate subscripts inside a value-proven function specialization.
pub(super) fn validate_specialized_subscripts(
    subscripts: &[Subscript],
    roles: &HashMap<VarName, PlannedRole>,
    values: &ShapeEnvironment,
) -> Result<(), ToDaeError> {
    let binders = HashSet::new();
    let states = HashSet::new();
    ExpressionValidator {
        roles,
        states: &states,
        binders: &binders,
        record_array_fields: None,
        values: Some(values),
    }
    .validate_subscripts(subscripts)
}

pub(super) fn validate_expression_with_record_array_fields(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    fields: &RecordArrayFieldPlans,
) -> Result<(), ToDaeError> {
    let binders = HashSet::new();
    ExpressionValidator {
        roles,
        states,
        binders: &binders,
        record_array_fields: Some(fields),
        values: None,
    }
    .validate(expression)
}

pub(super) fn validate_expression_scoped_with_record_array_fields(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    binders: &HashSet<VarName>,
    fields: &RecordArrayFieldPlans,
) -> Result<(), ToDaeError> {
    ExpressionValidator {
        roles,
        states,
        binders,
        record_array_fields: Some(fields),
        values: None,
    }
    .validate(expression)
}

impl<'a> ExpressionValidator<'a> {
    fn validate(self, expression: &Expression) -> Result<(), ToDaeError> {
        let span = expression_span(expression)?;
        match expression {
            Expression::Binary { op, lhs, rhs, .. } => {
                validate_binary_operator(op, span)?;
                self.validate(lhs)?;
                self.validate(rhs)
            }
            Expression::Unary { op, rhs, .. } => {
                validate_unary_operator(op, span)?;
                self.validate(rhs)
            }
            Expression::VarRef {
                name, subscripts, ..
            } => self.validate_reference(name, subscripts, span),
            Expression::BuiltinCall { function, args, .. } => {
                self.validate_builtin(*function, args, span)
            }
            Expression::Literal { .. } => Ok(()),
            Expression::If {
                branches,
                else_branch,
                ..
            } => self.validate_conditional(branches, else_branch, span),
            Expression::FunctionCall { args, .. } => self.validate_call_arguments(args),
            Expression::StringConversion { value, format, .. } => {
                self.validate(value)?;
                for operand in format.operands() {
                    self.validate(operand)?;
                }
                Ok(())
            }
            Expression::Array { elements, .. } => self.validate_array(elements, span),
            Expression::Range {
                start, step, end, ..
            } => self.validate_range(start, step.as_deref(), end, span),
            Expression::Index {
                base, subscripts, ..
            } => {
                self.validate(base)?;
                self.validate_subscripts(subscripts)
            }
            Expression::ArrayComprehension {
                expr,
                indices,
                filter,
                ..
            } => self.validate_array_comprehension(expr, indices, filter.as_deref(), span),
            Expression::FieldAccess { .. } => self.validate_field_access(expression, span),
            Expression::Tuple { .. } => Err(ToDaeError::unsupported_flat(
                "aggregate expression",
                "tuple lowering requires its typed semantic owner",
                span,
            )),
            Expression::Empty { .. } => Err(ToDaeError::unsupported_flat(
                "empty expression",
                "an absent semantic value cannot enter canonical DAE",
                span,
            )),
        }
    }

    fn validate_reference(
        self,
        name: &rumoca_core::Reference,
        subscripts: &[Subscript],
        span: Span,
    ) -> Result<(), ToDaeError> {
        if name.as_str() != "time"
            && !self.roles.contains_key(name.var_name())
            && !self.binders.contains(name.var_name())
        {
            return Err(ToDaeError::unresolved_reference(name.as_str(), span));
        }
        if self.binders.contains(name.var_name()) && !subscripts.is_empty() {
            return Err(ToDaeError::unsupported_flat(
                "structured-domain binder",
                "a domain binder is a scalar Integer coordinate and cannot be subscripted",
                span,
            ));
        }
        self.validate_subscripts(subscripts)
    }

    /// MLS §10.4.1: a compact range is either an Integer range whose bounds are
    /// settled at translation time or an enumeration range whose two bounds are
    /// literals of one enumeration type. Planned roles carry "is an enumeration
    /// literal" and the reference identities carry "declared by the same
    /// enumeration type", so neither answer comes from a rendered name.
    ///
    /// "Settled" is a literal in the model scope, and inside a value-proven
    /// function specialization it is additionally any bound that scope folds —
    /// MLS §12.2 lets a function write `1:integer(m/2)` over its input `m`, and
    /// the specialization that fixes `m` fixes the range with it.
    fn validate_range(
        self,
        start: &Expression,
        step: Option<&Expression>,
        end: &Expression,
        span: Span,
    ) -> Result<(), ToDaeError> {
        let is_enumeration_literal = |name: &rumoca_core::Reference| {
            matches!(
                self.roles.get(name.var_name()),
                Some(PlannedRole::EnumerationLiteral)
            )
        };
        if enumeration_range_type(start, step, end, &is_enumeration_literal).is_some() {
            return Ok(());
        }
        if has_enumeration_range_bound(start, end, &is_enumeration_literal) {
            return Err(ToDaeError::unsupported_flat(
                "enumeration range",
                "an enumeration compact range requires both bounds to be literals of the same \
                 enumeration type and no step",
                span,
            ));
        }
        self.require_static_bound(start, "range start")?;
        if let Some(step) = step {
            self.require_static_bound(step, "range step")?;
        }
        self.require_static_bound(end, "range end")
    }

    fn require_static_bound(self, bound: &Expression, owner: &str) -> Result<(), ToDaeError> {
        if self
            .values
            .is_some_and(|values| values.proven_extent(bound).is_some())
        {
            return Ok(());
        }
        require_integer_literal(bound, owner).map(|_| ())
    }

    fn validate_field_access(self, expression: &Expression, span: Span) -> Result<(), ToDaeError> {
        let Some(fields) = self.record_array_fields else {
            return Err(unsupported_record_field(span));
        };
        let Some(plan) = fields.get(expression) else {
            return Err(unsupported_record_field(span));
        };
        match plan {
            RecordArrayFieldPlan::MaterializedCoordinate { .. } => Ok(()),
            RecordArrayFieldPlan::Projection { subscripts, .. } => {
                self.validate_subscripts(subscripts)
            }
        }
    }
}

fn unsupported_record_field(span: Span) -> ToDaeError {
    ToDaeError::unsupported_flat(
        "aggregate expression",
        "record-field lowering requires its typed semantic owner",
        span,
    )
}

pub(super) fn validate_binary_operator(op: &OpBinary, span: Span) -> Result<(), ToDaeError> {
    if matches!(
        op,
        OpBinary::Add
            | OpBinary::Sub
            | OpBinary::Mul
            | OpBinary::Div
            | OpBinary::Eq
            | OpBinary::Neq
            | OpBinary::Lt
            | OpBinary::Le
            | OpBinary::Gt
            | OpBinary::Ge
            | OpBinary::And
            | OpBinary::Or
            | OpBinary::Exp
            | OpBinary::ExpElem
            | OpBinary::AddElem
            | OpBinary::SubElem
            | OpBinary::MulElem
            | OpBinary::DivElem
    ) {
        return Ok(());
    }
    Err(ToDaeError::unsupported_flat(
        "binary operator",
        format!("operator `{op}` has no scalar canonical DAE operation"),
        span,
    ))
}

pub(super) fn validate_unary_operator(op: &OpUnary, span: Span) -> Result<(), ToDaeError> {
    if matches!(
        op,
        OpUnary::Minus | OpUnary::Plus | OpUnary::Not | OpUnary::DotMinus | OpUnary::DotPlus
    ) {
        return Ok(());
    }
    Err(ToDaeError::unsupported_flat(
        "unary operator",
        format!("operator `{op}` has no scalar canonical DAE operation"),
        span,
    ))
}

impl ExpressionValidator<'_> {
    fn validate_conditional(
        self,
        branches: &[(Expression, Expression)],
        else_branch: &Expression,
        span: Span,
    ) -> Result<(), ToDaeError> {
        if branches.is_empty() {
            return Err(ToDaeError::unsupported_flat(
                "if expression",
                "a conditional expression requires at least one condition branch",
                span,
            ));
        }
        for (condition, value) in branches {
            self.validate(condition)?;
            self.validate(value)?;
        }
        self.validate(else_branch)
    }

    fn validate_call_arguments(self, arguments: &[Expression]) -> Result<(), ToDaeError> {
        for argument in arguments {
            if matches!(
                argument,
                Expression::Array {
                    elements,
                    ..
                } if elements.is_empty()
            ) {
                require_span(expression_span(argument)?, "empty function argument")?;
            } else {
                self.validate(argument)?;
            }
        }
        Ok(())
    }

    fn validate_array(self, elements: &[Expression], span: Span) -> Result<(), ToDaeError> {
        if elements.is_empty() {
            return Err(ToDaeError::unsupported_flat(
                "empty array",
                "an empty array needs an explicit checked element type",
                span,
            ));
        }
        for element in elements {
            self.validate(element)?;
        }
        Ok(())
    }

    fn validate_builtin(
        self,
        function: BuiltinFunction,
        arguments: &[Expression],
        span: Span,
    ) -> Result<(), ToDaeError> {
        if function == BuiltinFunction::Der {
            return self.validate_derivative(arguments, span);
        }
        if function == BuiltinFunction::Pre {
            return self.validate_pre(arguments, span);
        }
        if function == BuiltinFunction::Interval {
            if arguments.len() > 1 {
                return Err(ToDaeError::unsupported_runtime_operator(
                    function.name(),
                    "interval accepts at most one inference operand",
                    span,
                ));
            }
            return arguments
                .first()
                .map_or(Ok(()), |argument| self.validate(argument));
        }
        if matches!(function, BuiltinFunction::Hold | BuiltinFunction::Previous) {
            return self.validate_clocked_unary(function, arguments, span);
        }
        if function == BuiltinFunction::SemiLinear {
            return self.validate_semi_linear(arguments, span);
        }
        if !is_supported_builtin(function) {
            return Err(ToDaeError::unsupported_runtime_operator(
                function.name(),
                "no checked canonical owner exists for this operator in the active lowering slice",
                span,
            ));
        }
        for argument in arguments {
            self.validate(argument)?;
        }
        Ok(())
    }

    /// MLS §3.7.4.5 `semiLinear(x, positiveSlope, negativeSlope)`.
    ///
    /// The operator has a fixed three-operand contract; construction turns it
    /// into the checked conditional `if x >= 0 then positiveSlope*x else
    /// negativeSlope*x`, so every operand must itself be a lowerable
    /// expression.
    fn validate_semi_linear(self, arguments: &[Expression], span: Span) -> Result<(), ToDaeError> {
        let [x, positive_slope, negative_slope] = arguments else {
            return Err(ToDaeError::unsupported_runtime_operator(
                BuiltinFunction::SemiLinear.name(),
                "semiLinear takes exactly an operand and its positive and negative slopes",
                span,
            ));
        };
        self.validate(x)?;
        self.validate(positive_slope)?;
        self.validate(negative_slope)
    }

    fn validate_clocked_unary(
        self,
        function: BuiltinFunction,
        arguments: &[Expression],
        span: Span,
    ) -> Result<(), ToDaeError> {
        let [argument] = arguments else {
            return Err(ToDaeError::unsupported_runtime_operator(
                function.name(),
                "the checked clocked operator requires exactly one operand",
                span,
            ));
        };
        if function == BuiltinFunction::Previous {
            let Some((name, subscripts)) = derivative_reference(argument) else {
                return Err(invalid_reference_builtin("previous", function.name(), span));
            };
            if !matches!(
                self.roles.get(name.var_name()),
                Some(PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
            ) {
                return Err(ToDaeError::unsupported_flat(
                    "previous expression",
                    "previous(...) must name a discrete coordinate",
                    span,
                ));
            }
            return self.validate_subscripts(subscripts);
        }
        self.validate(argument)
    }

    fn validate_derivative(self, arguments: &[Expression], span: Span) -> Result<(), ToDaeError> {
        let [argument] = arguments else {
            return Err(invalid_reference_builtin("derivative", "der", span));
        };
        let Some((name, subscripts)) = derivative_reference(argument) else {
            return Err(invalid_reference_builtin("derivative", "der", span));
        };
        if !self.states.contains(name.var_name()) {
            return Err(ToDaeError::unsupported_flat(
                "derivative expression",
                "der(...) target is not a state coordinate",
                span,
            ));
        }
        self.validate_subscripts(subscripts)
    }

    fn validate_pre(self, arguments: &[Expression], span: Span) -> Result<(), ToDaeError> {
        let [argument] = arguments else {
            return Err(invalid_reference_builtin("pre", "pre", span));
        };
        let Some((name, subscripts)) = derivative_reference(argument) else {
            return Err(invalid_reference_builtin("pre", "pre", span));
        };
        if !matches!(
            self.roles.get(name.var_name()),
            Some(PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
        ) {
            return Err(ToDaeError::unsupported_flat(
                "pre expression",
                "pre(...) must name a discrete coordinate",
                span,
            ));
        }
        self.validate_subscripts(subscripts)
    }
}

fn invalid_reference_builtin(section: &str, name: &str, span: Span) -> ToDaeError {
    ToDaeError::unsupported_flat(
        format!("{section} expression"),
        format!("{name}(...) must have exactly one resolved variable-reference operand"),
        span,
    )
}

fn is_supported_builtin(function: BuiltinFunction) -> bool {
    matches!(
        function,
        BuiltinFunction::Abs
            | BuiltinFunction::Sign
            | BuiltinFunction::Sqrt
            | BuiltinFunction::Div
            | BuiltinFunction::Mod
            | BuiltinFunction::Rem
            | BuiltinFunction::Floor
            | BuiltinFunction::Ceil
            | BuiltinFunction::Integer
            | BuiltinFunction::Sin
            | BuiltinFunction::Cos
            | BuiltinFunction::Tan
            | BuiltinFunction::Asin
            | BuiltinFunction::Acos
            | BuiltinFunction::Atan
            | BuiltinFunction::Atan2
            | BuiltinFunction::Sinh
            | BuiltinFunction::Cosh
            | BuiltinFunction::Tanh
            | BuiltinFunction::Exp
            | BuiltinFunction::Log
            | BuiltinFunction::Log10
            | BuiltinFunction::Smooth
            | BuiltinFunction::NoEvent
            | BuiltinFunction::Homotopy
            | BuiltinFunction::Min
            | BuiltinFunction::Max
            | BuiltinFunction::Sum
            | BuiltinFunction::Product
            | BuiltinFunction::Size
            | BuiltinFunction::Zeros
            | BuiltinFunction::Ones
            | BuiltinFunction::Fill
            | BuiltinFunction::Linspace
            | BuiltinFunction::Cross
            | BuiltinFunction::Sample
            | BuiltinFunction::Clock
            | BuiltinFunction::Hold
            | BuiltinFunction::Previous
            | BuiltinFunction::Interval
            | BuiltinFunction::SubSample
            | BuiltinFunction::SuperSample
            | BuiltinFunction::ShiftSample
            | BuiltinFunction::BackSample
            | BuiltinFunction::NoClock
            | BuiltinFunction::Delay
    )
}

pub(super) fn validate_subscripts_scoped(
    subscripts: &[Subscript],
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    binders: &HashSet<VarName>,
) -> Result<(), ToDaeError> {
    ExpressionValidator {
        roles,
        states,
        binders,
        record_array_fields: None,
        values: None,
    }
    .validate_subscripts(subscripts)
}

impl ExpressionValidator<'_> {
    fn validate_subscripts(self, subscripts: &[Subscript]) -> Result<(), ToDaeError> {
        for subscript in subscripts {
            require_span(subscript.span(), "array subscript")?;
            match subscript {
                Subscript::Index { value, span } if *value < 1 => {
                    return Err(ToDaeError::unsupported_flat(
                        "array subscript",
                        "Modelica array indices are one-based positive integers",
                        *span,
                    ));
                }
                Subscript::Expr { expr, .. } => self.validate(expr)?,
                Subscript::Index { .. } | Subscript::Colon { .. } => {}
            }
        }
        Ok(())
    }

    fn validate_comprehension_range(self, expression: &Expression) -> Result<(), ToDaeError> {
        let Expression::Range {
            start, step, end, ..
        } = expression
        else {
            return Err(ToDaeError::unsupported_flat(
                "array comprehension domain",
                "a checked comprehension index requires an explicit range",
                expression_span(expression)?,
            ));
        };
        self.validate(start)?;
        if let Some(step) = step {
            self.validate(step)?;
        }
        self.validate(end)
    }

    fn validate_array_comprehension(
        self,
        body: &Expression,
        indices: &[rumoca_core::ComprehensionIndex],
        filter: Option<&Expression>,
        span: Span,
    ) -> Result<(), ToDaeError> {
        if filter.is_some() {
            return Err(ToDaeError::unsupported_flat(
                "filtered array comprehension",
                "canonical DAE requires an unfiltered rectangular domain",
                span,
            ));
        }
        // MLS §10.4.1 opens a comprehension index as a fresh scalar of the
        // comprehension, so it shadows any enclosing coordinate of the same flat
        // name. The proven-value scope is narrowed with the binder set, or a
        // bound written over an index would fold the shadowed coordinate's value.
        let mut binders = (*self.binders).clone();
        let mut values = self.values.cloned();
        for index in indices {
            ExpressionValidator {
                binders: &binders,
                values: values.as_ref(),
                ..self
            }
            .validate_comprehension_range(&index.range)?;
            binders.insert(VarName::new(&index.name));
            if let Some(values) = values.as_mut() {
                values.insert(VarName::new(&index.name), Vec::new());
            }
        }
        ExpressionValidator {
            binders: &binders,
            values: values.as_ref(),
            ..self
        }
        .validate(body)
    }
}

pub(super) fn require_integer_literal(
    expression: &Expression,
    owner: &str,
) -> Result<i64, ToDaeError> {
    if let Expression::Literal {
        value: Literal::Integer(value),
        ..
    } = expression
    {
        return Ok(*value);
    }
    Err(ToDaeError::unsupported_flat(
        owner,
        "the canonical compact range requires an integer literal bound",
        expression_span(expression)?,
    ))
}
