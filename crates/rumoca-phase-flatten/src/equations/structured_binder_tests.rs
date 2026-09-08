use super::*;

fn span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("structured_binder_test.mo"),
        3,
        4,
    )
}

fn reference(name: &str, local: bool) -> rumoca_core::Reference {
    let component = rumoca_core::ComponentReference::construct(
        local,
        span(),
        vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: span(),
            subs: Vec::new(),
            // This declaration identity is deliberately irrelevant to binder
            // selection: the domain-local typed ID is the semantic target.
            def_id: rumoca_core::DefId::new(91),
        }],
    )
    .expect("test source token has complete syntax provenance");
    rumoca_core::Reference::from_component_reference(component)
}

fn expression(reference: rumoca_core::Reference) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: reference,
        subscripts: Vec::new(),
        span: span(),
    }
}

fn rewritten_reference(expression: &rumoca_core::Expression) -> &rumoca_core::Reference {
    let rumoca_core::Expression::VarRef { name, .. } = expression else {
        panic!("binder annotation preserves a variable-reference node");
    };
    name
}

#[test]
fn binder_annotation_uses_typed_lexical_identity_not_declaration_identity() {
    let outer = rumoca_core::StructuredIndexBinderId::new(0);
    let inner = rumoca_core::StructuredIndexBinderId::new(1);
    let rewritten = annotate_structured_binder_targets(
        &expression(reference("i", false)),
        &[("i".to_string(), outer), ("i".to_string(), inner)],
    )
    .expect("a well-formed lexical binder occurrence is annotatable");

    assert_eq!(
        rewritten_reference(&rewritten).structured_binder(),
        Some(inner)
    );
}

#[test]
fn leading_dot_same_spelling_reference_does_not_collide_with_binder() {
    let rewritten = annotate_structured_binder_targets(
        &expression(reference("i", true)),
        &[(
            "i".to_string(),
            rumoca_core::StructuredIndexBinderId::new(0),
        )],
    )
    .expect("the explicit lookup escape needs no binder annotation");

    assert_eq!(rewritten_reference(&rewritten).structured_binder(), None);
}

#[test]
fn failed_binder_annotation_abandons_the_compact_template() {
    let occurrence = reference("i", false).with_instance_id(rumoca_core::InstanceId::new(7));
    let rewritten = annotate_structured_binder_targets(
        &expression(occurrence),
        &[(
            "i".to_string(),
            rumoca_core::StructuredIndexBinderId::new(0),
        )],
    );

    assert!(
        rewritten.is_none(),
        "an identity collision must select materialized rows, never emit an unannotated template"
    );
}
