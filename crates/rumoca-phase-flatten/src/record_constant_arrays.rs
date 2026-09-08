use std::sync::Arc;

use rumoca_ir_ast as ast;

use crate::{Context, FlattenError};

/// Whether the component's resolved type declaration is a record.
///
/// Derived predefined types are classes too, but their modifiers are scalar
/// attributes (for example `Frequency(start=50)`), not record field values.
/// The distinction is semantic and must follow the resolved declaration
/// identity rather than the rendered type name.
pub(crate) fn component_type_is_record(
    comp: &ast::Component,
    class_index: &ast::ClassDefIndex<'_>,
) -> bool {
    comp.type_def_id
        .and_then(|type_def_id| class_index.get(type_def_id))
        .is_some_and(|class_def| class_def.class_type == rumoca_core::ClassType::Record)
}

pub(crate) fn try_extract_record_array_constructor_constant(
    expr: &ast::Expression,
    class_index: &ast::ClassDefIndex<'_>,
    ctx: &mut Context,
    scope: &str,
    full_name: &str,
) -> Option<rumoca_core::Expression> {
    let ast::Expression::Array {
        elements,
        is_matrix,
        ..
    } = expr
    else {
        return None;
    };
    let mut evaluated = Vec::with_capacity(elements.len());
    for (idx, element) in elements.iter().enumerate() {
        let element_name = format!("{full_name}[{}]", idx + 1);
        let value = crate::try_extract_named_record_constructor_constant(
            element,
            class_index,
            ctx,
            scope,
            &element_name,
        )
        .or_else(|| {
            try_extract_record_array_constructor_constant(
                element,
                class_index,
                ctx,
                scope,
                &element_name,
            )
        })?;
        evaluated.push(value);
    }
    Some(rumoca_core::Expression::Array {
        elements: evaluated,
        is_matrix: *is_matrix,
        span: expr.span(),
    })
}

pub(crate) fn synthesize_component_modification_binding(
    comp: &ast::Component,
    class_index: &ast::ClassDefIndex<'_>,
) -> Result<Option<ast::Expression>, FlattenError> {
    if comp.modifications.is_empty()
        || record_modification_synthesis(comp, class_index)?
            == RecordModificationSynthesis::NotApplicable
    {
        return Ok(None);
    }
    synthesize_record_component_modification_binding(comp)
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum RecordModificationSynthesis {
    NotApplicable,
    Required,
}

fn record_modification_synthesis(
    comp: &ast::Component,
    class_index: &ast::ClassDefIndex<'_>,
) -> Result<RecordModificationSynthesis, FlattenError> {
    let Some(type_def_id) = comp.type_def_id else {
        // Resolve and Typecheck deliberately retain this exact shape for an
        // occurrence-specialized qualified type: the first segment has an
        // identity, while instantiation owns the selected trailing class.
        if comp.type_name.name.len() > 1 && comp.type_name.def_id.is_some() {
            return Ok(RecordModificationSynthesis::NotApplicable);
        }
        return missing_record_type_identity(comp);
    };
    if rumoca_core::BUILTIN_TYPES
        .iter()
        .any(|name| class_index.predefined_def_id(name) == Some(type_def_id))
    {
        return Ok(RecordModificationSynthesis::NotApplicable);
    }
    let Some(class_def) = class_index.get(type_def_id) else {
        return missing_record_type_identity(comp);
    };
    Ok(if class_def.class_type == rumoca_core::ClassType::Record {
        RecordModificationSynthesis::Required
    } else {
        RecordModificationSynthesis::NotApplicable
    })
}

fn missing_record_type_identity(
    comp: &ast::Component,
) -> Result<RecordModificationSynthesis, FlattenError> {
    let span = first_modification_span(comp)
        .ok_or_else(|| missing_synthesis_evidence(comp, "modifier source span"))?;
    Err(FlattenError::missing_resolved_class_metadata(
        &comp.name,
        "record modification synthesis requires an exact type declaration identity",
        span,
    ))
}

fn synthesize_each_array_component_modification_binding(
    comp: &ast::Component,
) -> Result<Option<ast::Expression>, FlattenError> {
    let [len] = comp.shape.as_slice() else {
        return Ok(None);
    };
    if comp.modifications.is_empty()
        || comp
            .modifications
            .keys()
            .any(|field| !comp.each_modifications.contains(field))
    {
        return Ok(None);
    }
    if *len == 0 {
        let span = first_modification_span(comp)
            .ok_or_else(|| missing_synthesis_evidence(comp, "modifier source span"))?;
        return Ok(Some(ast::Expression::Array {
            elements: Vec::new(),
            is_matrix: false,
            span,
        }));
    }
    let Some(scalar) = synthesize_scalar_component_modification_binding(comp)? else {
        return Ok(None);
    };
    let span = scalar.span();
    Ok(Some(ast::Expression::Array {
        elements: vec![scalar; *len],
        is_matrix: false,
        span,
    }))
}

fn synthesize_record_component_modification_binding(
    comp: &ast::Component,
) -> Result<Option<ast::Expression>, FlattenError> {
    if comp.shape.is_empty() {
        return synthesize_scalar_component_modification_binding(comp);
    }
    synthesize_each_array_component_modification_binding(comp)
}

fn synthesize_scalar_component_modification_binding(
    comp: &ast::Component,
) -> Result<Option<ast::Expression>, FlattenError> {
    if comp
        .modifications
        .values()
        .any(|modifier| modifier.component_modifier_binding_value().is_none())
    {
        // An attribute-only field modifier contributes no record value. The
        // whole shortcut is therefore inapplicable; publishing the remaining
        // fields would fabricate a partial record constructor.
        return Ok(None);
    }
    let span = first_modification_span(comp)
        .ok_or_else(|| missing_synthesis_evidence(comp, "modifier source span"))?;
    let type_def_id = comp
        .type_def_id
        .ok_or_else(|| missing_synthesis_evidence(comp, "resolved record type identity"))?;
    let type_name = comp.type_name.to_string();
    let location = required_synthesized_token_location(
        comp,
        comp.type_name.name.first().map(|token| &token.location),
    )?;
    let target = ast::ComponentReference {
        local: false,
        parts: vec![ast::ComponentRefPart {
            ident: rumoca_core::Token {
                text: Arc::from(type_name.as_str()),
                location,
                token_number: 0,
                token_type: 0,
            },
            subs: None,
            def_id: Some(type_def_id),
        }],
        span,
        qualified_display_name: Some(rumoca_core::VarName::new(type_name)),
    };
    let mut modifications = Vec::with_capacity(comp.modifications.len());
    for (field, modifier) in &comp.modifications {
        let value = modifier.component_modifier_binding_value().ok_or_else(|| {
            missing_synthesis_evidence(comp, "complete record field value bindings")
        })?;
        modifications.push(ast::Expression::NamedArgument {
            name: rumoca_core::Token {
                text: Arc::from(field.as_str()),
                location: required_synthesized_token_location(comp, modifier.get_location())?,
                token_number: 0,
                token_type: 0,
            },
            value: Arc::new(value.clone()),
            span: value.span(),
        });
    }
    let modification_count = modifications.len();
    Ok(Some(ast::Expression::ClassModification {
        target,
        modifications,
        each_flags: vec![false; modification_count],
        final_flags: vec![false; modification_count],
        redeclare_flags: vec![false; modification_count],
        span,
    }))
}

/// Select honest provenance for a token introduced by flattening.
///
/// A synthesized token has no source spelling of its own. Prefer the source
/// syntax that supplied its semantic value, then use the containing component
/// declaration as its explicit generated owner. A source-free location at this
/// boundary means validated AST construction was bypassed; accepting a dummy
/// location would launder that compiler bug into Flat IR. Parsed and future
/// synthesized components reaching this owner must therefore carry at least
/// one real declaration/type location.
fn required_synthesized_token_location(
    comp: &ast::Component,
    preferred: Option<&rumoca_core::Location>,
) -> Result<rumoca_core::Location, FlattenError> {
    for location in [
        preferred,
        Some(&comp.name_token.location),
        Some(&comp.location),
        comp.type_name.name.first().map(|token| &token.location),
    ]
    .into_iter()
    .flatten()
    {
        if location.has_source() {
            return Ok(location.clone());
        }
    }
    Err(missing_synthesis_evidence(comp, "source provenance"))
}

fn missing_synthesis_evidence(comp: &ast::Component, evidence: &str) -> FlattenError {
    FlattenError::Internal(format!(
        "record modification synthesis for component `{}` is missing {evidence}",
        comp.name
    ))
}

fn first_modification_span(comp: &ast::Component) -> Option<rumoca_core::Span> {
    comp.modifications
        .values()
        .next()
        .map(ast::Expression::span)
        .filter(|span| !span.is_dummy())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn location(source: &str, start: u32, end: u32) -> rumoca_core::Location {
        rumoca_core::Location {
            start_line: 1,
            start_column: start,
            end_line: 1,
            end_column: end,
            start,
            end,
            source: rumoca_core::SourceId::from_source_name(source),
        }
    }

    #[test]
    fn synthesized_record_modifier_uses_explicit_owner_provenance() {
        let owner = location("record_modifier.mo", 10, 20);
        let value_span = rumoca_core::Span::from_offsets(owner.source, 30, 32);
        let mut comp = ast::Component::empty_with_span(value_span);
        comp.type_def_id = Some(rumoca_core::DefId::new(7));
        comp.type_name = ast::Name::from_string("R");
        comp.name_token.location = owner.clone();
        comp.location = owner.clone();
        comp.modifications.insert(
            "emptyField".to_string(),
            ast::Expression::Array {
                elements: Vec::new(),
                is_matrix: false,
                span: value_span,
            },
        );

        let synthesized = synthesize_scalar_component_modification_binding(&comp)
            .expect("sourced synthesis has complete provenance")
            .expect("a sourced modifier synthesizes a constructor call");
        let ast::Expression::ClassModification {
            target,
            modifications,
            ..
        } = synthesized
        else {
            panic!("record modifications synthesize a class modification");
        };
        assert!(target.parts[0].ident.location.has_source());
        let ast::Expression::NamedArgument { name, .. } = &modifications[0] else {
            panic!("record fields synthesize named arguments");
        };
        assert_eq!(name.location, owner);
        assert!(name.location.has_source());
    }

    #[test]
    fn record_modifier_synthesis_cannot_reintroduce_default_locations() {
        let source = include_str!("record_constant_arrays.rs");
        for prohibited in [
            ["unwrap_or_", "default()"].concat(),
            ["Location::", "default()"].concat(),
            ["Default::", "default()"].concat(),
        ] {
            assert!(
                !source.contains(&prohibited),
                "record-modifier synthesis must not regain `{prohibited}`"
            );
        }
    }

    #[test]
    fn source_free_synthesized_component_is_not_admitted() {
        let mut comp = ast::Component::empty_with_span(rumoca_core::Span::DUMMY);
        comp.name = "r".to_string();
        comp.type_name = ast::Name::from_string("R");
        let error = required_synthesized_token_location(&comp, None)
            .expect_err("source-free synthesis must fail closed");
        assert!(
            matches!(error, FlattenError::Internal(message) if message.contains("source provenance"))
        );
    }

    #[test]
    fn unresolved_non_deferred_record_identity_is_a_typed_refusal() {
        let owner = location("record_modifier.mo", 10, 20);
        let modifier_span = rumoca_core::Span::from_offsets(owner.source, 30, 32);
        let mut comp = ast::Component::empty_with_span(modifier_span);
        comp.name = "r".to_string();
        comp.type_name = ast::Name::from_string("R");
        comp.modifications.insert(
            "field".to_string(),
            ast::Expression::Array {
                elements: Vec::new(),
                is_matrix: false,
                span: modifier_span,
            },
        );
        let tree = ast::ClassTree::new();
        let class_index = ast::ClassDefIndex::from_tree(&tree);

        let error = synthesize_component_modification_binding(&comp, &class_index)
            .expect_err("an unresolved simple type cannot be classified as non-record");
        assert!(matches!(
            error,
            FlattenError::MissingResolvedClassMetadata { span, .. } if span == modifier_span
        ));
    }

    #[test]
    fn occurrence_specialized_qualified_type_is_explicitly_not_applicable() {
        let owner = location("record_modifier.mo", 10, 20);
        let modifier_span = rumoca_core::Span::from_offsets(owner.source, 30, 32);
        let mut comp = ast::Component::empty_with_span(modifier_span);
        comp.name = "r".to_string();
        comp.type_name = ast::Name::from_string("Medium.R");
        comp.type_name.def_id = Some(rumoca_core::DefId::new(7));
        comp.modifications.insert(
            "field".to_string(),
            ast::Expression::Array {
                elements: Vec::new(),
                is_matrix: false,
                span: modifier_span,
            },
        );
        let tree = ast::ClassTree::new();
        let class_index = ast::ClassDefIndex::from_tree(&tree);

        assert!(
            synthesize_component_modification_binding(&comp, &class_index)
                .expect("occurrence-specialized type defers to its instantiated owner")
                .is_none()
        );
    }

    #[test]
    fn exact_predefined_type_identity_is_non_record() {
        let owner = location("record_modifier.mo", 10, 20);
        let modifier_span = rumoca_core::Span::from_offsets(owner.source, 30, 32);
        let mut tree = ast::ClassTree::new();
        crate::test_support::install_predefined_type_identities(&mut tree);
        let real = crate::test_support::predefined_type_def_id(&tree, "Real");
        let class_index = ast::ClassDefIndex::from_tree(&tree);
        let mut comp = ast::Component::empty_with_span(modifier_span);
        comp.name = "x".to_string();
        comp.type_def_id = Some(real);
        comp.type_name = ast::Name::from_string("Real");
        comp.modifications.insert(
            "nominal".to_string(),
            ast::Expression::Array {
                elements: Vec::new(),
                is_matrix: false,
                span: modifier_span,
            },
        );

        assert!(
            synthesize_component_modification_binding(&comp, &class_index)
                .expect("an exact predefined type is conclusively non-record")
                .is_none()
        );
    }

    #[test]
    fn attribute_only_modifier_prevents_partial_record_synthesis() {
        let owner = location("record_modifier.mo", 10, 20);
        let modifier_span = rumoca_core::Span::from_offsets(owner.source, 30, 32);
        let mut comp = ast::Component::empty_with_span(modifier_span);
        comp.name = "r".to_string();
        comp.type_def_id = Some(rumoca_core::DefId::new(7));
        comp.type_name = ast::Name::from_string("R");
        comp.name_token.location = owner.clone();
        comp.location = owner;
        comp.modifications.insert(
            "bound".to_string(),
            ast::Expression::Array {
                elements: Vec::new(),
                is_matrix: false,
                span: modifier_span,
            },
        );
        comp.modifications.insert(
            "attributesOnly".to_string(),
            ast::Expression::ClassModification {
                target: ast::ComponentReference {
                    local: false,
                    parts: Vec::new(),
                    span: modifier_span,
                    qualified_display_name: None,
                },
                modifications: Vec::new(),
                each_flags: Vec::new(),
                final_flags: Vec::new(),
                redeclare_flags: Vec::new(),
                span: modifier_span,
            },
        );

        assert!(
            synthesize_scalar_component_modification_binding(&comp)
                .expect("attribute-only fields make the shortcut inapplicable")
                .is_none(),
            "a bindable sibling must not escape as a partial constructor"
        );
    }

    #[test]
    fn zero_length_each_record_array_retains_its_empty_tensor_value() {
        let owner = location("record_modifier.mo", 10, 20);
        let modifier_span = rumoca_core::Span::from_offsets(owner.source, 30, 32);
        let mut comp = ast::Component::empty_with_span(modifier_span);
        comp.name = "records".to_string();
        comp.shape = vec![0];
        comp.modifications.insert(
            "field".to_string(),
            ast::Expression::Array {
                elements: Vec::new(),
                is_matrix: false,
                span: modifier_span,
            },
        );
        comp.each_modifications.insert("field".to_string());

        assert!(matches!(
            synthesize_record_component_modification_binding(&comp)
                .expect("a sourced zero domain retains its empty value"),
            Some(ast::Expression::Array {
                elements,
                is_matrix: false,
                span,
            }) if elements.is_empty() && span == modifier_span
        ));

        comp.each_modifications.clear();
        assert!(
            synthesize_record_component_modification_binding(&comp)
                .expect("non-each array modifiers remain owned by ordinary flattening")
                .is_none(),
            "an array-shaped record component must not fall through to scalar synthesis"
        );
    }
}
