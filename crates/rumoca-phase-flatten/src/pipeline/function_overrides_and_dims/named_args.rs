use super::*;

pub(super) fn named_function_arg_names(args: &[Expression]) -> FxHashSet<String> {
    args.iter()
        .filter_map(|arg| {
            let Expression::FunctionCall {
                name,
                args: named_args,
                is_constructor: _,
                span: _,
            } = arg
            else {
                return None;
            };
            (!named_args.is_empty())
                .then(|| name.as_str().strip_prefix("__rumoca_named_arg__."))
                .flatten()
                .map(str::to_string)
        })
        .collect()
}

pub(super) fn named_function_arg(
    name: &str,
    value: Expression,
    fallback_span: rumoca_core::Span,
) -> Expression {
    let span = value.span().unwrap_or(fallback_span);
    Expression::FunctionCall {
        name: rumoca_core::Reference::new(format!("__rumoca_named_arg__.{name}")),
        args: vec![value],
        is_constructor: true,
        span,
    }
}
