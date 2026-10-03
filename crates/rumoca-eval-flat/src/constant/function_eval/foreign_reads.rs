//! Executing a cataloged foreign file reader as a function body (SPEC_0040
//! FLAT-C06).

use rumoca_core::{Expression, Function};

use super::{EvalError, EvalState, FunctionEnv, Value, eval_expr_in_function};
use crate::constant::EvalEnvironment;
use crate::translation_reads::{ReadArgument, ReadElement, ResourceRoots, TranslationRead};

/// The catalog row an external function's body is, when the environment
/// evaluates parameter bindings at translation.
pub(super) fn translation_read<'a>(
    func: &Function,
    ctx: &'a dyn EvalEnvironment,
) -> Option<(TranslationRead, &'a ResourceRoots)> {
    let external = func.external.as_ref()?;
    let read = TranslationRead::from_external(
        &external.language,
        external
            .function_name
            .as_deref()
            .unwrap_or(func.name.last_segment()),
    )?;
    Some((read, ctx.translation_resources()?))
}

/// Bind the row's external arguments in the call environment, evaluate it,
/// and store what it writes in the function's outputs.
pub(super) fn execute(
    func: &Function,
    read: TranslationRead,
    resources: &ResourceRoots,
    env: &mut FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<(), EvalError> {
    let mismatch = || {
        EvalError::function_error(
            format!(
                "{} declares an external interface outside the `{}` catalog row",
                func.name,
                read.entry_point()
            ),
            eval.span,
        )
    };
    let external = func.external.as_ref().ok_or_else(mismatch)?;
    let interface = read.interface();
    if external.args.len() != interface.len()
        || external.output_name.is_some() != read.returns().is_some()
    {
        return Err(mismatch());
    }
    let mut inputs = Vec::new();
    let mut written = Vec::new();
    for (argument, role) in external.args.iter().zip(interface) {
        match *role {
            ReadArgument::Input(element) => {
                let value = eval_expr_in_function(argument, env, eval)?;
                if !has_element(&value, element) {
                    return Err(mismatch());
                }
                inputs.push(value);
            }
            ReadArgument::Output(..) => {
                written.push(output_formal(func, argument).ok_or_else(mismatch)?);
            }
        }
    }
    if let Some(output) = &external.output_name {
        written.push(output.clone());
    }
    let values = read
        .evaluate(&inputs, resources)
        .map_err(|error| EvalError::function_error(error.to_string(), eval.span))?;
    if values.len() != written.len() {
        return Err(mismatch());
    }
    for (name, value) in written.into_iter().zip(values) {
        env.outputs.insert(name, value);
    }
    Ok(())
}

fn has_element(value: &Value, element: ReadElement) -> bool {
    matches!(
        (value, element),
        (Value::String(_), ReadElement::String)
            | (Value::Integer(_), ReadElement::Integer)
            | (Value::Real(_), ReadElement::Real)
            | (Value::Bool(_), ReadElement::Boolean)
    )
}

/// The output formal an output argument position names, which is that formal
/// unsubscripted.
fn output_formal(func: &Function, argument: &Expression) -> Option<String> {
    let Expression::VarRef {
        name, subscripts, ..
    } = argument
    else {
        return None;
    };
    func.outputs
        .iter()
        .find(|output| subscripts.is_empty() && output.name == name.as_str())
        .map(|output| output.name.clone())
}
