use std::sync::Arc;

use rumoca_ir_ast as ast;

use crate::Context;

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
            ctx,
            scope,
            &element_name,
        )
        .or_else(|| {
            try_extract_record_array_constructor_constant(element, ctx, scope, &element_name)
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
) -> Option<ast::Expression> {
    if comp.modifications.is_empty() || !component_type_is_record(comp, class_index) {
        return None;
    }
    if let Some(array_binding) = synthesize_each_array_component_modification_binding(comp) {
        return Some(array_binding);
    }
    synthesize_scalar_component_modification_binding(comp)
}

fn synthesize_each_array_component_modification_binding(
    comp: &ast::Component,
) -> Option<ast::Expression> {
    let [len] = comp.shape.as_slice() else {
        return None;
    };
    if *len == 0
        || comp
            .modifications
            .keys()
            .any(|field| !comp.each_modifications.contains(field))
    {
        return None;
    }
    let scalar = synthesize_scalar_component_modification_binding(comp)?;
    let span = scalar.span();
    Some(ast::Expression::Array {
        elements: vec![scalar; *len],
        is_matrix: false,
        span,
    })
}

fn synthesize_scalar_component_modification_binding(
    comp: &ast::Component,
) -> Option<ast::Expression> {
    let span = first_modification_span(comp)?;
    let type_def_id = comp
        .type_def_id
        .expect("record modification synthesis requires an exactly resolved type declaration");
    let type_name = comp.type_name.to_string();
    let location = comp
        .type_name
        .name
        .first()
        .map(|token| token.location.clone())
        .unwrap_or_default();
    let target = ast::ComponentReference {
        local: false,
        parts: vec![ast::ComponentRefPart {
            ident: rumoca_core::Token {
                text: Arc::from(type_name.as_str()),
                location,
                ..Default::default()
            },
            subs: None,
            def_id: Some(type_def_id),
        }],
        span,
        qualified_display_name: Some(rumoca_core::VarName::new(type_name)),
    };
    let modifications: Vec<ast::Expression> = comp
        .modifications
        .iter()
        .filter_map(|(field, modifier)| {
            let value = modifier.component_modifier_binding_value()?;
            Some(ast::Expression::NamedArgument {
                name: rumoca_core::Token {
                    text: Arc::from(field.as_str()),
                    location: modifier.get_location().cloned().unwrap_or_default(),
                    ..Default::default()
                },
                value: Arc::new(value.clone()),
                span: value.span(),
            })
        })
        .collect();
    let modification_count = modifications.len();
    Some(ast::Expression::ClassModification {
        target,
        modifications,
        each_flags: vec![false; modification_count],
        final_flags: vec![false; modification_count],
        redeclare_flags: vec![false; modification_count],
        span,
    })
}

fn first_modification_span(comp: &ast::Component) -> Option<rumoca_core::Span> {
    comp.modifications
        .values()
        .next()
        .map(ast::Expression::span)
        .filter(|span| !span.is_dummy())
}
