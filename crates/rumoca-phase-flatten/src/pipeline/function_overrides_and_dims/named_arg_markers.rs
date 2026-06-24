use rumoca_core::Expression;

pub(super) fn preserve_named_arg_marker_shells(
    original_args: &[Expression],
    rewritten_args: Vec<Expression>,
) -> Vec<Expression> {
    original_args
        .iter()
        .zip(rewritten_args)
        .map(|(original, rewritten)| preserve_named_arg_marker_shell(original, rewritten))
        .collect()
}

pub(super) fn preserve_named_arg_marker_shell(
    original: &Expression,
    rewritten: Expression,
) -> Expression {
    let Expression::FunctionCall {
        name,
        is_constructor: true,
        span,
        ..
    } = original
    else {
        return rewritten;
    };
    if !name
        .as_str()
        .starts_with(rumoca_core::NAMED_FUNCTION_ARG_PREFIX)
    {
        return rewritten;
    }
    if matches!(
        &rewritten,
        Expression::FunctionCall {
            name: rewritten_name,
            ..
        } if rewritten_name.as_str() == name.as_str()
    ) {
        return rewritten;
    }
    Expression::FunctionCall {
        name: name.clone(),
        args: vec![rewritten],
        is_constructor: true,
        span: *span,
    }
}
