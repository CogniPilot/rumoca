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
    let returned = external.output_name.as_ref().zip(read.returns());
    for (name, value) in written.into_iter().zip(values) {
        let value = match returned {
            Some((output, ReadElement::Enumeration(literals))) if *output == name => {
                enumeration_value(func, output, literals, &value, eval).ok_or_else(mismatch)?
            }
            _ => value,
        };
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

/// The value of the enumeration output `output` whose one-based ordinal the
/// row returned: the literal the row names at that ordinal, of the canonical
/// enumeration type the output's declaration identifies (MLS §4.9.5), never
/// of its source spelling.
fn enumeration_value(
    func: &Function,
    output: &str,
    literals: &[&str],
    ordinal: &Value,
    eval: &EvalState<'_>,
) -> Option<Value> {
    let formal = func.outputs.iter().find(|formal| formal.name == output)?;
    let type_name = eval.ctx.enumeration_type(formal.type_def_id?)?;
    let Value::Integer(ordinal) = ordinal else {
        return None;
    };
    let literal = literals.get(usize::try_from(*ordinal).ok()?.checked_sub(1)?)?;
    eval.ctx
        .get_enum(&format!("{type_name}.{literal}"))
        .map(|(type_name, literal)| Value::Enum(type_name.clone(), literal.clone()))
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
