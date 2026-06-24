//! Narrow owner for qualified class-name parsing in instantiate.
//!
//! Instantiation resolves component types through `name_map`/`def_map`
//! tables keyed by qualified dotted class names. Walking those names stays
//! centralized here; semantic code must not re-derive class nesting with ad
//! hoc string splitting. Prefer `ast::Name` parts, `DefId`s, and
//! `QualifiedName` structure whenever they are available.

/// Split a qualified class name into `(enclosing scope, leaf name)`.
pub(crate) fn class_scope_split(name: &str) -> Option<(&str, &str)> {
    rumoca_core::split_last_top_level(name)
}

/// The leaf segment of a qualified class name.
pub(crate) fn class_name_leaf(name: &str) -> &str {
    rumoca_core::top_level_last_segment(name)
}

/// Build a component reference expression from a dotted path.
pub(crate) fn component_ref_expr_from_dotted(
    value: &str,
    span: rumoca_core::Span,
) -> rumoca_ir_ast::Expression {
    rumoca_ir_ast::Expression::ComponentReference(rumoca_ir_ast::ComponentReference {
        local: false,
        parts: rumoca_core::split_path_with_indices(value)
            .into_iter()
            .map(|part| rumoca_ir_ast::ComponentRefPart {
                ident: rumoca_core::Token {
                    text: part.to_string().into(),
                    ..Default::default()
                },
                subs: None,
            })
            .collect(),
        def_id: None,
        span,
    })
}
