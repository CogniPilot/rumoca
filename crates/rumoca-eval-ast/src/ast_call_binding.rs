use rumoca_ir_ast::{ClassDef, Component, Expression};

/// One input formal paired with its explicit actual, if the call supplied one.
pub(crate) struct BoundAstInput<'function, 'argument> {
    pub(crate) name: &'function str,
    pub(crate) component: &'function Component,
    pub(crate) argument: Option<&'argument Expression>,
}

/// Validate and bind the source-level positional/named call layout.
///
/// Positional actuals form a prefix. Named actuals form a unique, known suffix.
/// Every omitted formal must have a declaration binding. The returned vector is
/// in formal order so evaluators can bind explicit actuals before evaluating
/// defaults that refer to those inputs.
pub(crate) fn bind_ast_function_call<'function, 'argument>(
    function: &'function ClassDef,
    arguments: &'argument [Expression],
) -> Option<Vec<BoundAstInput<'function, 'argument>>> {
    let inputs: Vec<_> = function
        .components
        .iter()
        .filter(|(_, component)| matches!(component.causality, rumoca_core::Causality::Input(_)))
        .collect();
    let mut bound = vec![None; inputs.len()];
    let mut next_positional = 0usize;
    let mut seen_named = false;
    for argument in arguments {
        if let Expression::NamedArgument { name, value, .. } = argument {
            seen_named = true;
            let index = inputs
                .iter()
                .position(|(input_name, _)| input_name.as_str() == name.text.as_ref())?;
            if bound[index].replace(value.as_ref()).is_some() {
                return None;
            }
            continue;
        }
        if seen_named {
            return None;
        }
        let slot = bound.get_mut(next_positional)?;
        if slot.replace(argument).is_some() {
            return None;
        }
        next_positional += 1;
    }

    inputs
        .into_iter()
        .zip(bound)
        .map(|((name, component), argument)| {
            if argument.is_none() && component.binding.is_none() {
                return None;
            }
            Some(BoundAstInput {
                name,
                component,
                argument,
            })
        })
        .collect()
}
