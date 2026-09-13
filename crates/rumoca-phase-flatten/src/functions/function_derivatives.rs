//! Resolved MLS §12.7.1 source derivative contracts.

use super::*;

#[derive(Debug)]
pub(super) struct ParsedDerivativeAnnotation {
    pub(super) derivative_function: rumoca_core::Reference,
    pub(super) order: u32,
    pub(super) zero_derivative: Vec<rumoca_core::Reference>,
    pub(super) no_derivative: Vec<rumoca_core::Reference>,
}

struct DerivativeSyntax<'a> {
    function: &'a ast::Expression,
    modifiers: Vec<&'a ast::Expression>,
}

pub(super) fn extract_derivative_annotations(
    annotations: &[ast::Expression],
) -> Result<Vec<ParsedDerivativeAnnotation>, FlattenError> {
    let mut result = Vec::new();
    for expression in annotations {
        let Some(syntax) = derivative_syntax(expression)? else {
            continue;
        };
        let mut annotation = ParsedDerivativeAnnotation {
            derivative_function: resolved_reference(syntax.function)?,
            order: 1,
            zero_derivative: Vec::new(),
            no_derivative: Vec::new(),
        };
        let mut has_order = false;
        for modifier in syntax.modifiers {
            apply_derivative_modifier(modifier, &mut annotation, &mut has_order)?;
        }
        result.push(annotation);
    }
    Ok(result)
}

fn derivative_syntax(
    expression: &ast::Expression,
) -> Result<Option<DerivativeSyntax<'_>>, FlattenError> {
    let syntax = match expression {
        ast::Expression::NamedArgument { name, value, .. }
            if name.text.as_ref() == "derivative" =>
        {
            DerivativeSyntax {
                function: value,
                modifiers: Vec::new(),
            }
        }
        ast::Expression::Modification { target, value, .. }
            if annotation_name(target) == Some("derivative") =>
        {
            let modifiers = target.parts[0]
                .subs
                .iter()
                .flatten()
                .map(|subscript| {
                    let ast::Subscript::Expression(expression) = subscript else {
                        return Err(FlattenError::invalid_derivative_annotation(
                            "expected a derivative restriction",
                            target.span,
                        ));
                    };
                    Ok(expression)
                })
                .collect::<Result<_, _>>()?;
            DerivativeSyntax {
                function: value,
                modifiers,
            }
        }
        ast::Expression::Binary {
            op: rumoca_core::OpBinary::Assign,
            lhs,
            rhs,
            ..
        } => {
            let ast::Expression::ClassModification {
                target,
                modifications,
                ..
            } = lhs.as_ref()
            else {
                return Ok(None);
            };
            if annotation_name(target) != Some("derivative") {
                return Ok(None);
            }
            DerivativeSyntax {
                function: rhs,
                modifiers: modifications.iter().collect(),
            }
        }
        ast::Expression::ClassModification {
            target,
            modifications,
            ..
        } if annotation_name(target) == Some("derivative") => {
            let mut functions = modifications
                .iter()
                .filter(|value| matches!(value, ast::Expression::ComponentReference(_)));
            let function = functions.next().ok_or_else(|| {
                FlattenError::invalid_derivative_annotation(
                    "missing derivative function",
                    expression.span(),
                )
            })?;
            if functions.next().is_some() {
                return Err(FlattenError::invalid_derivative_annotation(
                    "multiple derivative functions in one annotation",
                    expression.span(),
                ));
            }
            DerivativeSyntax {
                function,
                modifiers: modifications
                    .iter()
                    .filter(|value| !matches!(value, ast::Expression::ComponentReference(_)))
                    .collect(),
            }
        }
        _ => return Ok(None),
    };
    Ok(Some(syntax))
}

fn apply_derivative_modifier(
    expression: &ast::Expression,
    annotation: &mut ParsedDerivativeAnnotation,
    has_order: &mut bool,
) -> Result<(), FlattenError> {
    let (name, value) = match expression {
        ast::Expression::Modification { target, value, .. } => {
            (annotation_name(target), value.as_ref())
        }
        ast::Expression::NamedArgument { name, value, .. } => {
            (Some(name.text.as_ref()), value.as_ref())
        }
        _ => {
            return Err(FlattenError::invalid_derivative_annotation(
                "expected an assigned derivative restriction",
                expression.span(),
            ));
        }
    };
    match name {
        Some("order") => {
            if *has_order {
                return Err(FlattenError::invalid_derivative_annotation(
                    "derivative order specified more than once",
                    expression.span(),
                ));
            }
            let ast::Expression::Terminal {
                terminal_type: ast::TerminalType::UnsignedInteger,
                token,
                ..
            } = value
            else {
                return Err(FlattenError::invalid_derivative_annotation(
                    "derivative order must be a positive integer literal",
                    value.span(),
                ));
            };
            annotation.order = token
                .text
                .parse::<u32>()
                .ok()
                .filter(|order| *order > 0)
                .ok_or_else(|| {
                    FlattenError::invalid_derivative_annotation(
                        "derivative order must be a positive u32 integer",
                        value.span(),
                    )
                })?;
            *has_order = true;
        }
        Some("zeroDerivative") => annotation.zero_derivative.push(resolved_reference(value)?),
        Some("noDerivative") => annotation.no_derivative.push(resolved_reference(value)?),
        _ => {
            return Err(FlattenError::invalid_derivative_annotation(
                "unknown derivative restriction",
                expression.span(),
            ));
        }
    }
    Ok(())
}

fn annotation_name(reference: &ast::ComponentReference) -> Option<&str> {
    let [part] = reference.parts.as_slice() else {
        return None;
    };
    Some(part.ident.text.as_ref())
}

fn resolved_reference(
    expression: &ast::Expression,
) -> Result<rumoca_core::Reference, FlattenError> {
    let ast::Expression::ComponentReference(reference) = expression else {
        return Err(FlattenError::invalid_derivative_annotation(
            "expected a resolved reference",
            expression.span(),
        ));
    };
    if reference.parts.is_empty() || reference.parts.iter().any(|part| part.def_id.is_none()) {
        return Err(FlattenError::invalid_derivative_annotation(
            "annotation reference lacks declaration identity",
            expression.span(),
        ));
    }
    let request = FunctionRequest::from_resolved_ast_reference(String::new(), reference);
    Ok(rumoca_core::Reference::from_component_reference(
        request
            .component_ref
            .expect("resolved annotation reference"),
    ))
}

pub(super) fn lower_derivative_annotations(
    annotations: &[ast::Expression],
    inputs: &[rumoca_core::FunctionParam],
) -> Result<Vec<rumoca_core::DerivativeAnnotation>, FlattenError> {
    extract_derivative_annotations(annotations)?
        .into_iter()
        .map(|annotation| lower_derivative_annotation(annotation, inputs))
        .collect()
}

fn lower_derivative_annotation(
    annotation: ParsedDerivativeAnnotation,
    inputs: &[rumoca_core::FunctionParam],
) -> Result<rumoca_core::DerivativeAnnotation, FlattenError> {
    use rumoca_core::FunctionDerivativeInput as Role;
    let mut roles = vec![Role::Differentiate; inputs.len()];
    for (references, role) in [
        (&annotation.zero_derivative, Role::ZeroDerivative),
        (&annotation.no_derivative, Role::NoDerivative),
    ] {
        for reference in references {
            let ordinal = inputs
                .iter()
                .position(|input| input.def_id == reference.target_def_id())
                .ok_or_else(|| {
                    FlattenError::invalid_derivative_annotation(
                        format!(
                            "restriction does not identify a function input: {}",
                            reference.as_str()
                        ),
                        reference.span().expect("resolved restriction occurrence"),
                    )
                })?;
            if roles[ordinal] != Role::Differentiate {
                return Err(FlattenError::invalid_derivative_annotation(
                    "duplicate derivative input restriction",
                    reference.span().expect("resolved restriction occurrence"),
                ));
            }
            roles[ordinal] = role;
        }
    }
    Ok(rumoca_core::DerivativeAnnotation {
        derivative_function: annotation.derivative_function,
        order: annotation.order,
        inputs: roles,
    })
}
