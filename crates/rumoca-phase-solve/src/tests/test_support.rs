pub(crate) fn component_ref_from_name(name: &str) -> rumoca_core::ComponentReference {
    let span = generated_fixture_component_ref_span();
    rumoca_core::ComponentReference {
        local: false,
        span,
        parts: rumoca_core::split_path_with_indices(name)
            .into_iter()
            .map(|segment| component_ref_part_from_segment(segment, span))
            .collect(),
        def_id: None,
    }
}

pub(crate) fn fixture_key_for_component_ref(
    reference: &rumoca_core::ComponentReference,
    display_name: &str,
) -> Option<rumoca_ir_solve::ComponentReferenceKey> {
    (reference.span == generated_fixture_component_ref_span() && reference.def_id.is_none())
        .then(|| rumoca_ir_solve::ComponentReferenceKey::generated(display_name))
}

pub(crate) fn fixture_key_for_reference(
    reference: &rumoca_core::Reference,
) -> Option<rumoca_ir_solve::ComponentReferenceKey> {
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
    match variable.component_ref.as_ref() {
        Some(component_ref) => fixture_key_for_component_ref(component_ref, name),
        None if variable.source_span.is_dummy() => {
            Some(rumoca_ir_solve::ComponentReferenceKey::generated(name))
        }
        None => None,
    }
}

fn generated_fixture_component_ref_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_generated_fixture_component_ref.mo"),
        1,
        2,
    )
}

fn component_ref_part_from_segment(
    segment: &str,
    span: rumoca_core::Span,
) -> rumoca_core::ComponentRefPart {
    match rumoca_core::parse_scalar_name(segment) {
        Some(scalar) => rumoca_core::ComponentRefPart {
            ident: scalar.base.to_string(),
            span,
            subs: scalar
                .indices
                .iter()
                .copied()
                .map(|index| rumoca_core::Subscript::generated_index(index, span))
                .collect(),
        },
        None => rumoca_core::ComponentRefPart {
            ident: segment.to_string(),
            span,
            subs: Vec::new(),
        },
    }
}
