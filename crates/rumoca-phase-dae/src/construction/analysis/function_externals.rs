//! MLS §12.9 external function interface analysis.
//!
//! Flat keeps the declared external clause verbatim. This module proves that
//! clause is a checkable DAE interface: an exact language, an exact entry
//! point, an ordered argument list whose positions are either closed input
//! expressions over the function's own formals or declared outputs the foreign
//! body writes, and exactly one producer for every declared output.
use super::*;

pub(in crate::construction) struct ExternalFunctionPlan {
    pub(in crate::construction) purity: dae::FunctionPurity,
    pub(in crate::construction) language: dae::ExternalLanguage,
    pub(in crate::construction) symbol: VarName,
    pub(in crate::construction) arguments: Vec<ExternalArgumentPlan>,
    pub(in crate::construction) result: Option<VarName>,
    pub(in crate::construction) linkage: dae::ExternalLinkage,
}

pub(in crate::construction) enum ExternalArgumentPlan {
    /// Ordinal into `Function::external.args` of a closed input expression.
    Input(usize),
    /// Declared output the foreign body writes at this ABI position.
    Output(VarName),
}

pub(super) fn validate_external_function(
    function: &rumoca_core::Function,
    context: FunctionValidationContext<'_>,
) -> Result<ExternalFunctionPlan, ToDaeError> {
    let external = function
        .external
        .as_ref()
        .expect("external analysis runs only for declared external functions");
    if function.is_constructor {
        return Err(ToDaeError::unsupported_flat(
            "external function interface",
            format!(
                "`{}` cannot be both a record constructor and external",
                function.name
            ),
            function.span,
        ));
    }
    let language = dae::ExternalLanguage::from_declared(&external.language).ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "external function language",
            format!(
                "`{}` declares external language `{}`, which MLS §12.9 does not define",
                function.name, external.language
            ),
            function.span,
        )
    })?;
    let symbol = external_symbol(function, external)?;
    let input_roles = external_input_roles(function, context.flat);
    let mut produced: HashMap<VarName, Span> = HashMap::new();
    let mut arguments = Vec::with_capacity(external.args.len());
    for (ordinal, argument) in external.args.iter().enumerate() {
        match declared_output(function, argument) {
            Some(output) => {
                record_produced_output(&mut produced, &output, function, function.span)?;
                arguments.push(ExternalArgumentPlan::Output(output));
            }
            None => {
                reject_mutable_value_argument(function, argument)?;
                validate_function_expression_with_roles(argument, &input_roles, context.flat)?;
                arguments.push(ExternalArgumentPlan::Input(ordinal));
            }
        }
    }
    let result = match &external.output_name {
        Some(name) => {
            let output = VarName::new(name);
            if !function
                .outputs
                .iter()
                .any(|value| output.as_str() == value.name)
            {
                return Err(ToDaeError::unsupported_flat(
                    "external function result",
                    format!(
                        "`{}` binds external result `{}`, which is not a declared output",
                        function.name, name
                    ),
                    function.span,
                ));
            }
            record_produced_output(&mut produced, &output, function, function.span)?;
            Some(output)
        }
        None => None,
    };
    for output in &function.outputs {
        if !produced.contains_key(&VarName::new(&output.name)) {
            return Err(ToDaeError::unsupported_flat(
                "external function interface",
                format!(
                    "`{}` declares output `{}` that its external body never produces",
                    function.name, output.name
                ),
                output.span,
            ));
        }
    }
    Ok(ExternalFunctionPlan {
        purity: if function.pure {
            dae::FunctionPurity::Pure
        } else {
            dae::FunctionPurity::Impure
        },
        language,
        symbol,
        arguments,
        result,
        linkage: external_linkage(function, external)?,
    })
}

/// MLS §12.9: the entry point is the exact declared external name.
///
/// When the declaration omits it, MLS defaults the entry point to the
/// function's own *simple* name. Flat stores only the flattened path, and
/// recovering a simple name from that rendered text is exactly the
/// rendered-name fallback this compiler forbids, so the omitted form fails
/// until Flat preserves the declared simple name structurally.
fn external_symbol(
    function: &rumoca_core::Function,
    external: &rumoca_core::ExternalFunction,
) -> Result<VarName, ToDaeError> {
    let Some(declared) = &external.function_name else {
        return Err(ToDaeError::unsupported_flat(
            "external function entry point",
            format!(
                "`{}` omits its external entry point name and Flat does not preserve the \
                 declared simple name",
                function.name
            ),
            function.span,
        ));
    };
    if declared.is_empty() {
        return Err(ToDaeError::unsupported_flat(
            "external function entry point",
            format!("`{}` has no external entry point name", function.name),
            function.span,
        ));
    }
    Ok(VarName::new(declared))
}

/// Roles legal inside an external argument expression.
///
/// Only the declared formal inputs and enumeration literals are in scope: an
/// external body has no Modelica statement that could have defined an output
/// or a protected local before the call.
fn external_input_roles(
    function: &rumoca_core::Function,
    flat: &flat::Model,
) -> HashMap<VarName, PlannedRole> {
    let mut roles = function
        .inputs
        .iter()
        .map(|parameter| (VarName::new(&parameter.name), PlannedRole::Parameter))
        .collect::<HashMap<_, _>>();
    for value in &function.inputs {
        roles.extend(
            record_field_projections(value, flat)
                .into_iter()
                .map(|(path, _)| (path, PlannedRole::Parameter)),
        );
    }
    for literal in flat.enum_literal_ordinals.keys() {
        roles.insert(VarName::new(literal), PlannedRole::EnumerationLiteral);
    }
    roles
}

/// A whole, unsubscripted reference to a declared output at an ABI position.
fn declared_output(function: &rumoca_core::Function, argument: &Expression) -> Option<VarName> {
    let Expression::VarRef {
        name, subscripts, ..
    } = argument
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    let target = name.var_name();
    function
        .outputs
        .iter()
        .any(|value| target.as_str() == value.name)
        .then(|| target.clone())
}

/// Protected locals have no value at the external call and are not outputs.
fn reject_mutable_value_argument(
    function: &rumoca_core::Function,
    argument: &Expression,
) -> Result<(), ToDaeError> {
    let Expression::VarRef { name, span, .. } = argument else {
        return Ok(());
    };
    let target = name.var_name();
    if function
        .locals
        .iter()
        .any(|value| target.as_str() == value.name)
    {
        return Err(ToDaeError::unsupported_flat(
            "external function argument",
            format!(
                "`{}` passes protected local `{target}` to its external body without a checked owner",
                function.name
            ),
            *span,
        ));
    }
    Ok(())
}

fn record_produced_output(
    produced: &mut HashMap<VarName, Span>,
    output: &VarName,
    function: &rumoca_core::Function,
    span: Span,
) -> Result<(), ToDaeError> {
    if produced.insert(output.clone(), span).is_some() {
        return Err(ToDaeError::unsupported_flat(
            "external function interface",
            format!(
                "`{}` produces output `{output}` at more than one external position",
                function.name
            ),
            span,
        ));
    }
    Ok(())
}

/// MLS §12.9.4 link facts, read from the declared annotation values exactly.
fn external_linkage(
    function: &rumoca_core::Function,
    external: &rumoca_core::ExternalFunction,
) -> Result<dae::ExternalLinkage, ToDaeError> {
    let mut libraries = Vec::new();
    let mut include = None;
    let mut include_directory = None;
    let mut library_directory = None;
    for annotation in &external.annotations {
        let [name] = annotation.name.as_slice() else {
            continue;
        };
        match name.as_str() {
            "Library" => libraries.extend(string_facts(function, annotation)?),
            "Include" => include = Some(single_string_fact(function, annotation)?),
            "IncludeDirectory" => {
                include_directory = Some(single_string_fact(function, annotation)?);
            }
            "LibraryDirectory" => {
                library_directory = Some(single_string_fact(function, annotation)?);
            }
            _ => {}
        }
    }
    Ok(dae::ExternalLinkage::new(
        libraries,
        include,
        include_directory,
        library_directory,
    ))
}

fn string_facts(
    function: &rumoca_core::Function,
    annotation: &rumoca_core::ExternalFunctionAnnotation,
) -> Result<Vec<String>, ToDaeError> {
    match &annotation.value {
        Expression::Array { elements, .. } => elements
            .iter()
            .map(|element| string_fact(function, annotation, element))
            .collect(),
        value => Ok(vec![string_fact(function, annotation, value)?]),
    }
}

fn single_string_fact(
    function: &rumoca_core::Function,
    annotation: &rumoca_core::ExternalFunctionAnnotation,
) -> Result<String, ToDaeError> {
    string_fact(function, annotation, &annotation.value)
}

fn string_fact(
    function: &rumoca_core::Function,
    annotation: &rumoca_core::ExternalFunctionAnnotation,
    value: &Expression,
) -> Result<String, ToDaeError> {
    match value {
        Expression::Literal {
            value: rumoca_core::Literal::String(text),
            ..
        } => Ok(text.clone()),
        _ => Err(ToDaeError::unsupported_flat(
            "external function link facts",
            format!(
                "`{}` declares `{}` with a value that is not a String literal",
                function.name,
                annotation.name.join(".")
            ),
            annotation.span,
        )),
    }
}
