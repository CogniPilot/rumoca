use super::*;
use rumoca_core::{ComponentReference, Reference, Span};

pub(super) fn component_reference_from_path(
    path: &str,
    span: Span,
    def_id: Option<DefId>,
) -> ComponentReference {
    ComponentReference::from_flat_segments(path, span, def_id)
}

/// Convert a component reference to a VarRef with optional def-map canonicalization.
///
/// For enum literals resolved by `def_id`, canonicalize to the fully-qualified
/// declaration path so aliases like `L.'1'` become e.g.
/// `Modelica.Electrical.Digital.Interfaces.Logic.'1'`.
pub(super) fn from_component_ref_with_def_map_impl(
    cr: &ast::ComponentReference,
    def_map: Option<&IndexMap<DefId, String>>,
) -> Expression {
    if cr.parts.is_empty()
        && let Some(def_id) = cr.def_id
        && let Some(path) = def_map.and_then(|map| map.get(&def_id))
    {
        let component_ref = component_reference_from_path(path, cr.span, Some(def_id));
        return Expression::VarRef {
            name: Reference::from_component_reference(component_ref),
            subscripts: vec![],
            span: cr.span,
        };
    }

    if cr.parts.iter().all(|part| part.subs.is_none())
        && let Some(def_id) = cr.def_id
        && let Some(path) = def_map.and_then(|map| map.get(&def_id))
        && is_enum_literal_ref(cr, path)
    {
        let component_ref = component_reference_from_path(path, cr.span, Some(def_id));
        return Expression::VarRef {
            name: Reference::from_component_reference(component_ref),
            subscripts: vec![],
            span: cr.span,
        };
    }

    if let Some(index_expr) = component_ref_with_dynamic_final_subscripts(cr) {
        return index_expr;
    }

    expression_from_component_ref(cr)
}

fn component_ref_with_dynamic_final_subscripts(cr: &ast::ComponentReference) -> Option<Expression> {
    let last = cr.parts.last()?;
    let last_subs_ast = last.subs.as_ref()?;
    if last_subs_ast.is_empty() {
        return None;
    }

    let last_subs: Vec<Subscript> = last_subs_ast
        .iter()
        .map(|sub| subscript_from_ast(sub, cr.span))
        .collect();
    let has_dynamic = last_subs
        .iter()
        .any(|sub| !matches!(sub, Subscript::Index { .. }));
    if !has_dynamic {
        return None;
    }

    let mut base_ref = component_reference_from_ast_with_def_map(cr, None);
    if let Some(last) = base_ref.parts.last_mut() {
        last.subs.clear();
    }

    Some(Expression::Index {
        base: Box::new(Expression::VarRef {
            name: Reference::from_component_reference(base_ref),
            subscripts: vec![],
            span: cr.span,
        }),
        subscripts: last_subs,
        span: cr.span,
    })
}

/// Detect enum literal references that should be canonicalized through `def_id`.
fn is_enum_literal_ref(cr: &ast::ComponentReference, canonical_path: &str) -> bool {
    let Some(last_part) = cr.parts.last() else {
        return false;
    };

    let textual_literal = last_part.ident.text.as_ref();
    if !is_quoted_identifier(textual_literal) {
        return false;
    }

    rumoca_core::ComponentPath::from_flat_path(canonical_path)
        .parts()
        .last()
        .is_some_and(|segment| is_quoted_identifier(segment))
}

/// Modelica quoted identifiers use single quotes (e.g., `'1'`).
fn is_quoted_identifier(name: &str) -> bool {
    name.starts_with('\'') && name.ends_with('\'') && name.len() >= 2
}
