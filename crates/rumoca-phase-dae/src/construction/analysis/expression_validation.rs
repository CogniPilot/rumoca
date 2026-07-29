use super::*;

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

pub(super) fn validate_conditional(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    binders: &HashSet<VarName>,
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
        validate_expression_scoped(condition, roles, states, binders)?;
        validate_expression_scoped(value, roles, states, binders)?;
    }
    validate_expression_scoped(else_branch, roles, states, binders)
}

pub(super) fn validate_array(
    elements: &[Expression],
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    binders: &HashSet<VarName>,
    span: Span,
) -> Result<(), ToDaeError> {
    if elements.is_empty() {
        return Err(ToDaeError::unsupported_flat(
            "empty array",
            "an empty array needs an explicit checked element type",
            span,
        ));
    }
    for element in elements {
        validate_expression_scoped(element, roles, states, binders)?;
    }
    Ok(())
}

pub(super) fn validate_builtin(
    function: BuiltinFunction,
    args: &[Expression],
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    binders: &HashSet<VarName>,
    span: Span,
) -> Result<(), ToDaeError> {
    if function == BuiltinFunction::Der {
        let [argument] = args else {
            return Err(ToDaeError::unsupported_flat(
                "derivative expression",
                "der(...) must have exactly one resolved variable-reference operand",
                span,
            ));
        };
        let Some((name, subscripts)) = derivative_reference(argument) else {
            return Err(ToDaeError::unsupported_flat(
                "derivative expression",
                "der(...) must have exactly one resolved variable-reference operand",
                span,
            ));
        };
        if !states.contains(name.var_name()) {
            return Err(ToDaeError::unsupported_flat(
                "derivative expression",
                "der(...) target is not a state coordinate",
                span,
            ));
        }
        return validate_subscripts_scoped(subscripts, roles, states, binders);
    }
    if function == BuiltinFunction::Pre {
        let [argument] = args else {
            return Err(ToDaeError::unsupported_flat(
                "pre expression",
                "pre(...) must have exactly one resolved variable-reference operand",
                span,
            ));
        };
        let Some((name, subscripts)) = derivative_reference(argument) else {
            return Err(ToDaeError::unsupported_flat(
                "pre expression",
                "pre(...) must have exactly one resolved variable-reference operand",
                span,
            ));
        };
        if !matches!(
            roles.get(name.var_name()),
            Some(PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
        ) {
            return Err(ToDaeError::unsupported_flat(
                "pre expression",
                "pre(...) must name a discrete coordinate",
                span,
            ));
        }
        return validate_subscripts_scoped(subscripts, roles, states, binders);
    }
    let supported = matches!(
        function,
        BuiltinFunction::Abs
            | BuiltinFunction::Sign
            | BuiltinFunction::Sqrt
            | BuiltinFunction::Mod
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
    );
    if !supported {
        return Err(ToDaeError::unsupported_runtime_operator(
            function.name(),
            "no checked canonical owner exists for this operator in the active lowering slice",
            span,
        ));
    }
    for argument in args {
        validate_expression_scoped(argument, roles, states, binders)?;
    }
    Ok(())
}

pub(super) fn validate_subscripts_scoped(
    subscripts: &[Subscript],
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    binders: &HashSet<VarName>,
) -> Result<(), ToDaeError> {
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
            Subscript::Expr { expr, .. } => {
                validate_expression_scoped(expr, roles, states, binders)?
            }
            Subscript::Index { .. } | Subscript::Colon { .. } => {}
        }
    }
    Ok(())
}

pub(super) fn validate_comprehension_range(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    binders: &HashSet<VarName>,
) -> Result<(), ToDaeError> {
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
    validate_expression_scoped(start, roles, states, binders)?;
    if let Some(step) = step {
        validate_expression_scoped(step, roles, states, binders)?;
    }
    validate_expression_scoped(end, roles, states, binders)
}

pub(super) fn validate_array_comprehension(
    body: &Expression,
    indices: &[rumoca_core::ComprehensionIndex],
    filter: Option<&Expression>,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    binders: &HashSet<VarName>,
    span: Span,
) -> Result<(), ToDaeError> {
    if filter.is_some() {
        return Err(ToDaeError::unsupported_flat(
            "filtered array comprehension",
            "canonical DAE requires an unfiltered rectangular domain",
            span,
        ));
    }
    let mut comprehension_binders = binders.clone();
    for index in indices {
        validate_comprehension_range(&index.range, roles, states, &comprehension_binders)?;
        comprehension_binders.insert(VarName::new(&index.name));
    }
    validate_expression_scoped(body, roles, states, &comprehension_binders)
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
