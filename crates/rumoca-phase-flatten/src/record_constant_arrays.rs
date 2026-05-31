use std::sync::Arc;

use rumoca_ir_ast as ast;

use crate::Context;

pub(crate) fn is_record_like_type(type_name: &str) -> bool {
    !matches!(
        type_name,
        "Real" | "Integer" | "Boolean" | "String" | "Enumeration"
    )
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
) -> Option<ast::Expression> {
    if comp.modifications.is_empty() {
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
    let target = ast::ComponentReference {
        local: false,
        parts: comp
            .type_name
            .name
            .iter()
            .cloned()
            .map(|ident| ast::ComponentRefPart { ident, subs: None })
            .collect(),
        def_id: comp.type_name.def_id,
        span: first_modification_span(comp)?,
    };
    let modifications: Vec<ast::Expression> = comp
        .modifications
        .iter()
        .map(|(field, value)| ast::Expression::NamedArgument {
            name: rumoca_core::Token {
                text: Arc::from(field.as_str()),
                location: value.get_location().cloned().unwrap_or_default(),
                ..Default::default()
            },
            value: Arc::new(value.clone()),
            span: value.span(),
        })
        .collect();
    let modification_count = modifications.len();
    Some(ast::Expression::ClassModification {
        target,
        modifications,
        each_flags: vec![false; modification_count],
        final_flags: vec![false; modification_count],
        redeclare_flags: vec![false; modification_count],
        span: first_modification_span(comp)?,
    })
}

fn first_modification_span(comp: &ast::Component) -> Option<rumoca_core::Span> {
    comp.modifications
        .values()
        .next()
        .map(ast::Expression::span)
}
