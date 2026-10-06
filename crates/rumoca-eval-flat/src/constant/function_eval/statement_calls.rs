use super::*;

pub(super) fn eval_fn_call_stmt(
    comp: &Reference,
    args: &[Expression],
    outputs: &[Option<ComponentReference>],
    env: &mut FunctionEnv,
    eval: &EvalState<'_>,
    span: Span,
) -> Result<FlowControl, EvalError> {
    let func_name = comp.as_str();
    if func_name == "assert" {
        let (condition, message, level) = match args {
            [condition, message] => (condition, message, None),
            [condition, message, level] => (condition, message, Some(level)),
            _ => {
                return Err(EvalError::WrongArgCount {
                    expected: 2,
                    actual: args.len(),
                    span,
                });
            }
        };
        if !outputs.is_empty() {
            return Err(EvalError::FunctionError {
                message: "assert has no result".into(),
                span,
            });
        }
        return eval_assert_stmt(condition, message, level, env, eval, span);
    }
    match func_name {
        "print" | "terminate" | "Modelica.Utilities.Streams.print" => {
            return Ok(FlowControl::Continue);
        }
        _ => {}
    }
    let arg_values = args
        .iter()
        .map(|arg| eval_expr_in_function(arg, env, eval))
        .collect::<Result<_, _>>()?;
    let result = call_function(func_name, arg_values, eval)?;
    if !outputs.is_empty() {
        assign_fn_outputs(outputs, result, env, eval)?;
    }
    Ok(FlowControl::Continue)
}

/// MLS §8.3.7 applies to assertions in algorithms as well as equations. A
/// selected error assertion aborts evaluation; a true assertion never evaluates
/// its message. Warning actions cannot disappear into a value-only constant.
pub(super) fn eval_assert_stmt(
    condition: &Expression,
    message: &Expression,
    level: Option<&Expression>,
    env: &mut FunctionEnv,
    eval: &EvalState<'_>,
    span: Span,
) -> Result<FlowControl, EvalError> {
    let condition = eval_expr_in_function(condition, env, eval)?;
    let condition = condition
        .as_bool()
        .ok_or_else(|| EvalError::type_mismatch("Boolean", format!("{condition:?}"), span))?;
    if condition {
        return Ok(FlowControl::Continue);
    }
    if let Some(level) = level {
        let level = eval_expr_in_function(level, env, eval)?;
        match level {
            Value::Enum(ref type_name, ref literal)
                if type_name == "AssertionLevel" && literal == "warning" =>
            {
                return Err(EvalError::not_constant(
                    "warning assertion requires a retained reporting action",
                    span,
                ));
            }
            Value::Enum(ref type_name, ref literal)
                if type_name == "AssertionLevel" && literal == "error" => {}
            value => {
                return Err(EvalError::type_mismatch(
                    "AssertionLevel",
                    format!("{value:?}"),
                    span,
                ));
            }
        }
    }
    let message = eval_expr_in_function(message, env, eval)?;
    let Value::String(message) = message else {
        return Err(EvalError::type_mismatch(
            "String",
            format!("{message:?}"),
            span,
        ));
    };
    Err(EvalError::FunctionError { message, span })
}
