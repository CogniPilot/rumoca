use super::*;

pub(super) fn project_reference_field_path_and_indices(
    name: &rumoca_core::Reference,
    field_path: &[String],
    indices: &[usize],
    span: rumoca_core::Span,
) -> Result<rumoca_core::Reference, LowerError> {
    let span = if span.is_dummy() {
        name.span().unwrap_or(span)
    } else {
        span
    };
    if field_path.is_empty() && indices.is_empty() {
        return Ok(name.clone());
    }
    let mut component_ref = name
        .component_ref()
        .cloned()
        .unwrap_or_else(|| fallback_component_reference(name.as_str(), span));
    for field in field_path {
        component_ref.parts.push(rumoca_core::ComponentRefPart {
            ident: field.clone(),
            span,
            subs: Vec::new(),
        });
    }
    let last = component_ref.parts.last_mut().ok_or_else(|| {
        LowerError::contract_violation(
            format!(
                "array projection for `{}` has an empty component reference",
                name.as_str()
            ),
            span,
        )
    })?;
    let mut subscripts =
        projection_vec_with_capacity(indices.len(), "projected reference subscript count", span)?;
    for index in indices {
        subscripts.push(projected_reference_subscript(*index, span)?);
    }
    last.subs.extend(subscripts);
    Ok(rumoca_core::Reference::from_component_reference(
        component_ref,
    ))
}

fn fallback_component_reference(
    name: &str,
    span: rumoca_core::Span,
) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: false,
        span,
        parts: vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span,
            subs: Vec::new(),
        }],
        def_id: None,
    }
}

fn projected_reference_subscript(
    index: usize,
    span: rumoca_core::Span,
) -> Result<rumoca_core::Subscript, LowerError> {
    checked_generated_subscript_from_usize(index, span, "projected target reference subscript")
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
