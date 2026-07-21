use rumoca_core::{Expression, ExpressionRewriter, FunctionParam, NAMED_FUNCTION_ARG_PREFIX};

/// Bind positional and named arguments to the resolved function signature.
///
/// Explicit arguments remain in caller scope. Only defaults are rewritten
/// through earlier formal bindings, as required by MLS section 12.4.1.
pub(crate) fn bind_function_arguments(
    inputs: &[FunctionParam],
    args: &[Expression],
) -> Option<Vec<Expression>> {
    let (positional, named) = split_arguments(args)?;
    if positional.len() > inputs.len()
        || named
            .iter()
            .any(|(name, _)| !inputs.iter().any(|input| input.name == *name))
    {
        return None;
    }

    let mut bindings = Vec::with_capacity(inputs.len());
    for (index, input) in inputs.iter().enumerate() {
        let named_value = named
            .iter()
            .find_map(|(name, value)| (*name == input.name).then_some(*value));
        if index < positional.len() && named_value.is_some() {
            return None;
        }
        let value = if let Some(value) = positional.get(index).copied().or(named_value) {
            value.clone()
        } else {
            let default = input.default.as_ref()?;
            FormalSubstituter {
                inputs,
                bindings: &bindings,
            }
            .rewrite_expression(default)
        };
        bindings.push(value);
    }
    Some(bindings)
}

type NamedArgument<'a> = (&'a str, &'a Expression);

fn split_arguments(args: &[Expression]) -> Option<(Vec<&Expression>, Vec<NamedArgument<'_>>)> {
    let mut positional = Vec::new();
    let mut named = Vec::new();
    for arg in args {
        match decode_named_argument(arg)? {
            Some((name, value)) => {
                if named.iter().any(|(existing, _)| *existing == name) {
                    return None;
                }
                named.push((name, value));
            }
            None if named.is_empty() => positional.push(arg),
            None => return None,
        }
    }
    Some((positional, named))
}

fn decode_named_argument(expr: &Expression) -> Option<Option<NamedArgument<'_>>> {
    let Expression::FunctionCall { name, args, .. } = expr else {
        return Some(None);
    };
    let Some(name) = name.as_str().strip_prefix(NAMED_FUNCTION_ARG_PREFIX) else {
        return Some(None);
    };
    let [value] = args.as_slice() else {
        return None;
    };
    Some(Some((name, value)))
}

struct FormalSubstituter<'a> {
    inputs: &'a [FunctionParam],
    bindings: &'a [Expression],
}

impl ExpressionRewriter for FormalSubstituter<'_> {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        let Expression::VarRef {
            name,
            subscripts,
            span,
        } = expr
        else {
            return self.walk_expression(expr);
        };
        if !subscripts.is_empty() {
            return self.walk_expression(expr);
        }
        self.inputs
            .iter()
            .position(|input| input.name == name.as_str())
            .and_then(|index| self.bindings.get(index))
            .cloned()
            .map(|value| value.with_span(*span))
            .unwrap_or_else(|| self.walk_expression(expr))
    }
}

#[cfg(test)]
mod tests {
    use rumoca_core::{Literal, Reference, Span};

    use super::*;

    fn integer(value: i64) -> Expression {
        Expression::Literal {
            value: Literal::Integer(value),
            span: Span::DUMMY,
        }
    }

    fn input(name: &str, default: Option<Expression>) -> FunctionParam {
        let mut input = FunctionParam::new(name, "Real", Span::DUMMY);
        input.default = default;
        input
    }

    fn named(name: &str, value: Expression) -> Expression {
        Expression::FunctionCall {
            name: Reference::new(format!("{NAMED_FUNCTION_ARG_PREFIX}{name}")),
            args: vec![value],
            is_constructor: true,
            span: Span::DUMMY,
        }
    }

    #[test]
    fn binds_constructor_default_after_positional_argument() {
        let inputs = vec![input("re", None), input("im", Some(integer(0)))];
        assert_eq!(
            bind_function_arguments(&inputs, &[integer(3)]),
            Some(vec![integer(3), integer(0)])
        );
    }

    #[test]
    fn substitutes_earlier_formals_only_inside_defaults() {
        let formal_ref = Expression::VarRef {
            name: Reference::new("first"),
            subscripts: Vec::new(),
            span: Span::DUMMY,
        };
        let caller_ref = formal_ref.clone();
        let inputs = vec![input("first", None), input("second", Some(formal_ref))];

        assert_eq!(
            bind_function_arguments(&inputs, &[integer(7)]),
            Some(vec![integer(7), integer(7)])
        );
        assert_eq!(
            bind_function_arguments(&inputs, &[integer(7), caller_ref.clone()]),
            Some(vec![integer(7), caller_ref])
        );
    }

    #[test]
    fn binds_named_arguments_by_formal_name() {
        let inputs = vec![input("re", None), input("im", Some(integer(0)))];
        assert_eq!(
            bind_function_arguments(&inputs, &[named("im", integer(4)), named("re", integer(2))]),
            Some(vec![integer(2), integer(4)])
        );
    }
}
