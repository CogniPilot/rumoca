use super::*;

pub(super) fn project_reference_indices(
    name: &rumoca_core::Reference,
    indices: &[usize],
    span: rumoca_core::Span,
) -> Result<rumoca_core::Reference, LowerError> {
    project_reference_field_path_and_indices(name, &[], indices, span)
}

pub(super) fn project_reference_field_path_and_indices(
    name: &rumoca_core::Reference,
    field_path: &[String],
    indices: &[usize],
    span: rumoca_core::Span,
) -> Result<rumoca_core::Reference, LowerError> {
    if field_path.is_empty() && indices.is_empty() {
        return Ok(name.clone());
    }
    let component_ref = name
        .component_ref()
        .ok_or_else(|| LowerError::ContractViolation {
            reason: format!(
                "array projection for `{}` lost structured component-reference metadata",
                name.as_str()
            ),
            span,
        })?;
    let mut component_ref = component_ref.clone();
    for field in field_path {
        component_ref.parts.push(rumoca_core::ComponentRefPart {
            ident: field.clone(),
            span,
            subs: Vec::new(),
        });
    }
    let last = component_ref
        .parts
        .last_mut()
        .ok_or_else(|| LowerError::ContractViolation {
            reason: format!(
                "array projection for `{}` has an empty component reference",
                name.as_str()
            ),
            span,
        })?;
    last.subs.extend(
        indices
            .iter()
            .copied()
            .map(|index| rumoca_core::Subscript::generated_index(index as i64, span)),
    );
    Ok(rumoca_core::Reference::from_component_reference(
        component_ref,
    ))
}

pub(super) fn projected_target_binding_key(
    base: &str,
    field_path: &[String],
    indices: &[usize],
) -> String {
    let field_base = field_path.iter().fold(base.to_string(), |prefix, field| {
        format!("{prefix}.{field}")
    });
    if indices.is_empty() {
        return field_base;
    }
    dae::format_subscript_key(&field_base, indices)
}
