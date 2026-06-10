pub(crate) fn component_ref_from_name(name: &str) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference {
        local: false,
        span: rumoca_core::Span::DUMMY,
        parts: rumoca_core::split_path_with_indices(name)
            .into_iter()
            .map(component_ref_part_from_segment)
            .collect(),
        def_id: None,
    }
}

pub(crate) fn fixture_key_for_component_ref(
    reference: &rumoca_core::ComponentReference,
    display_name: &str,
) -> Option<rumoca_ir_solve::ComponentReferenceKey> {
    (reference.span.is_dummy() && reference.def_id.is_none())
        .then(|| rumoca_ir_solve::ComponentReferenceKey::generated(display_name))
}

pub(crate) fn fixture_key_for_reference(
    reference: &rumoca_core::Reference,
    span: rumoca_core::Span,
) -> Option<rumoca_ir_solve::ComponentReferenceKey> {
    if !span.is_dummy() {
        return None;
    }
    match reference.component_ref() {
        Some(component_ref) => fixture_key_for_component_ref(component_ref, reference.as_str()),
        None => Some(rumoca_ir_solve::ComponentReferenceKey::generated(
            reference.as_str(),
        )),
    }
}

pub(crate) fn fixture_key_for_variable(
    name: &str,
    variable: &rumoca_ir_dae::Variable,
) -> Option<rumoca_ir_solve::ComponentReferenceKey> {
    if !variable.source_span.is_dummy() {
        return None;
    }
    match variable.component_ref.as_ref() {
        Some(component_ref) => fixture_key_for_component_ref(component_ref, name),
        None => Some(rumoca_ir_solve::ComponentReferenceKey::generated(name)),
    }
}

fn component_ref_part_from_segment(segment: &str) -> rumoca_core::ComponentRefPart {
    match rumoca_core::parse_scalar_name(segment) {
        Some(scalar) => rumoca_core::ComponentRefPart {
            ident: scalar.base.to_string(),
            span: rumoca_core::Span::DUMMY,
            subs: scalar
                .indices
                .iter()
                .copied()
                .map(|index| {
                    rumoca_core::Subscript::generated_index(index, rumoca_core::Span::DUMMY)
                })
                .collect(),
        },
        None => rumoca_core::ComponentRefPart {
            ident: segment.to_string(),
            span: rumoca_core::Span::DUMMY,
            subs: Vec::new(),
        },
    }
}
