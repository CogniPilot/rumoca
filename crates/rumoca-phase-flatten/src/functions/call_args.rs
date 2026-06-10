//! MLS §12.4.1 argument-slot validation for calls to collected functions.

use rumoca_ir_flat as flat;

use crate::errors::FlattenError;

/// MLS §12.4.1 argument-slot validation for calls to collected functions:
/// no duplicate named slots, no named argument for a slot already filled
/// positionally, no unknown named arguments, and no unfilled slots without
/// a default (FUNC-019/FUNC-020).
pub(crate) fn validate_flat_function_call_args(flat: &flat::Model) -> Result<(), FlattenError> {
    use rumoca_core::ExpressionVisitor;

    let mut walker = CallArgsWalker {
        functions: &flat.functions,
        variables: &flat.variables,
        error: None,
        call_arg_depth: 0,
    };
    for eq in flat.equations.iter().chain(flat.initial_equations.iter()) {
        walker.visit_expression(&eq.residual);
    }
    for var in flat.variables.values() {
        if let Some(binding) = var.binding.as_ref() {
            walker.visit_expression(binding);
        }
    }
    match walker.error {
        Some(error) => Err(error),
        None => Ok(()),
    }
}

struct CallArgsWalker<'a> {
    functions: &'a flat::VarNameIndexMap<rumoca_core::Function>,
    variables: &'a flat::VarNameIndexMap<flat::Variable>,
    error: Option<FlattenError>,
    /// Depth of nested function-argument positions. MLS §12.4.2 partial
    /// application means a call appearing as a functional argument may
    /// deliberately leave inputs unbound, so the unfilled-slot rule only
    /// applies at depth zero.
    call_arg_depth: usize,
}

impl rumoca_core::ExpressionVisitor for CallArgsWalker<'_> {
    fn visit_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
    ) {
        let is_named_marker = name
            .as_str()
            .starts_with(rumoca_core::NAMED_FUNCTION_ARG_PREFIX);
        if self.error.is_none()
            && !is_constructor
            && !is_named_marker
            && let Some(function) = self
                .functions
                .get(&rumoca_core::VarName::new(name.as_str()))
            && !function.is_constructor
        {
            let allow_unfilled = self.call_arg_depth > 0;
            self.error = validate_call_slots(function, name.as_str(), args, allow_unfilled)
                .and_then(|()| {
                    validate_vectorized_dimensions(function, name.as_str(), args, self.variables)
                })
                .err();
        }
        if !is_named_marker {
            self.call_arg_depth += 1;
        }
        self.walk_function_call(name, args, is_constructor);
        if !is_named_marker {
            self.call_arg_depth -= 1;
        }
    }
}

fn validate_call_slots(
    function: &rumoca_core::Function,
    call_name: &str,
    args: &[rumoca_core::Expression],
    allow_unfilled: bool,
) -> Result<(), FlattenError> {
    let span = function.span;
    let mut positional = 0usize;
    let mut named: Vec<&str> = Vec::new();
    for arg in args {
        if let rumoca_core::Expression::FunctionCall { name, .. } = arg
            && let Some(slot) = name
                .as_str()
                .strip_prefix(rumoca_core::NAMED_FUNCTION_ARG_PREFIX)
        {
            if named.contains(&slot) {
                return Err(FlattenError::invalid_function_call_args(
                    call_name,
                    format!("named argument slot `{slot}` is filled more than once"),
                    span,
                ));
            }
            named.push(slot);
        } else {
            if !named.is_empty() {
                return Err(FlattenError::invalid_function_call_args(
                    call_name,
                    "positional arguments may not follow named arguments",
                    span,
                ));
            }
            positional += 1;
        }
    }

    let inputs = &function.inputs;
    if positional > inputs.len() {
        return Err(FlattenError::invalid_function_call_args(
            call_name,
            format!(
                "{positional} positional argument(s) for {} input slot(s)",
                inputs.len()
            ),
            span,
        ));
    }
    for slot in &named {
        let Some(index) = inputs.iter().position(|input| input.name == *slot) else {
            return Err(FlattenError::invalid_function_call_args(
                call_name,
                format!("named argument `{slot}` does not match any input"),
                span,
            ));
        };
        if index < positional {
            return Err(FlattenError::invalid_function_call_args(
                call_name,
                format!("named argument `{slot}` fills a slot already taken positionally"),
                span,
            ));
        }
    }
    for (index, input) in inputs.iter().enumerate() {
        if allow_unfilled {
            break;
        }
        let filled = index < positional || named.contains(&input.name.as_str());
        if !filled && input.default.is_none() {
            return Err(FlattenError::invalid_function_call_args(
                call_name,
                format!("input `{}` has no argument and no default", input.name),
                span,
            ));
        }
    }
    Ok(())
}

/// MLS §12.4.6 / FUNC-027: when a call is vectorized (array arguments where
/// the function declares lower-dimensional inputs), the extra leading
/// dimensions must agree across all vectorized arguments. Only plain,
/// unsubscripted variable references are checked; other argument forms have
/// no statically known dimensions here.
fn validate_vectorized_dimensions(
    function: &rumoca_core::Function,
    call_name: &str,
    args: &[rumoca_core::Expression],
    variables: &flat::VarNameIndexMap<flat::Variable>,
) -> Result<(), FlattenError> {
    let mut vectorized: Option<(Vec<i64>, String)> = None;
    for (index, arg) in args
        .iter()
        .filter(|arg| !is_named_arg_marker(arg))
        .enumerate()
    {
        let Some(input) = function.inputs.get(index) else {
            break;
        };
        let rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } = arg
        else {
            continue;
        };
        if !subscripts.is_empty() {
            continue;
        }
        let Some(variable) = variables.get(name.var_name()) else {
            continue;
        };
        if variable.dims.len() <= input.dims.len() {
            continue;
        }
        let extra = variable.dims[..variable.dims.len() - input.dims.len()].to_vec();
        match &vectorized {
            None => vectorized = Some((extra, name.as_str().to_string())),
            Some((expected, first_name)) if *expected != extra => {
                return Err(FlattenError::invalid_function_call_args(
                    call_name,
                    format!(
                        "vectorized arguments must share dimensions: `{first_name}` adds {expected:?} but `{}` adds {extra:?} (MLS §12.4.6)",
                        name.as_str()
                    ),
                    function.span,
                ));
            }
            Some(_) => {}
        }
    }
    Ok(())
}

fn is_named_arg_marker(arg: &rumoca_core::Expression) -> bool {
    matches!(
        arg,
        rumoca_core::Expression::FunctionCall { name, .. }
            if name.as_str().starts_with(rumoca_core::NAMED_FUNCTION_ARG_PREFIX)
    )
}
