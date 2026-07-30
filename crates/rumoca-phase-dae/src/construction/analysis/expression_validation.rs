use super::*;

#[derive(Clone, Copy)]
struct ExpressionValidator<'a> {
    roles: &'a HashMap<VarName, PlannedRole>,
    states: &'a HashSet<VarName>,
    binders: &'a HashSet<VarName>,
    record_array_fields: Option<&'a RecordArrayFieldPlans>,
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
    }
    .validate(expression)
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
            } => {
                require_integer_literal(start, "range start")?;
                if let Some(step) = step {
                    require_integer_literal(step, "range step")?;
                }
                require_integer_literal(end, "range end")?;
                Ok(())
            }
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
            Expression::FieldAccess { .. } => self.validate_field_access(span),
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

    fn validate_field_access(self, span: Span) -> Result<(), ToDaeError> {
        let Some(fields) = self.record_array_fields else {
            return Err(unsupported_record_field(span));
        };
        let Some(plan) = fields.get(&span) else {
            return Err(unsupported_record_field(span));
        };
        if let Some(coordinate) = plan
            .coordinates
            .iter()
            .find(|coordinate| !self.roles.contains_key(*coordinate))
        {
            return Err(ToDaeError::unsupported_flat(
                "record-array member slice",
                format!("planned materialized coordinate `{coordinate}` has no DAE role"),
                span,
            ));
        }
        self.validate_subscripts(&plan.subscripts)
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
        let mut binders = (*self.binders).clone();
        for index in indices {
            ExpressionValidator {
                binders: &binders,
                ..self
            }
            .validate_comprehension_range(&index.range)?;
            binders.insert(VarName::new(&index.name));
        }
        ExpressionValidator {
            binders: &binders,
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
