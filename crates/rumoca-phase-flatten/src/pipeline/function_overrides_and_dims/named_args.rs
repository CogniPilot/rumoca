use super::*;

pub(super) fn named_function_arg_names(args: &[Expression]) -> FxHashSet<String> {
    args.iter()
        .filter_map(|arg| {
            let Expression::FunctionCall {
                name,
                args: named_args,
                is_constructor: _,
                span: _,
                ..
            } = arg
            else {
                return None;
            };
            (!named_args.is_empty())
                .then(|| {
                    name.as_str()
                        .strip_prefix(rumoca_core::NAMED_FUNCTION_ARG_PREFIX)
                })
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
        name: rumoca_core::Reference::generated(format!(
            "{}{name}",
            rumoca_core::NAMED_FUNCTION_ARG_PREFIX
        )),
        args: vec![value],
        is_constructor: true,
        call_kind: rumoca_core::FunctionCallKind::Invocation,
        span,
    }
}
