use super::*;

/// Convert a component reference to a VarRef with optional def-map canonicalization.
///
/// For enum literals resolved by `def_id`, canonicalize to the fully-qualified
/// declaration path so aliases like `L.'1'` become e.g.
/// `Modelica.Electrical.Digital.Interfaces.Logic.'1'`.
pub(super) fn from_component_ref_with_def_map_impl(
    cr: &ast::ComponentReference,
    def_map: Option<&IndexMap<ast::DefId, String>>,
) -> Expression {
    if cr.parts.is_empty()
        && let Some(def_id) = cr.def_id
        && let Some(path) = def_map.and_then(|map| map.get(&def_id))
    {
        return Expression::VarRef {
            name: VarName::new(path.clone()),
            subscripts: vec![],
        };
    }

    if cr.parts.iter().all(|part| part.subs.is_none())
        && let Some(def_id) = cr.def_id
        && let Some(path) = def_map.and_then(|map| map.get(&def_id))
        && is_enum_literal_ref(cr, path)
    {
        return Expression::VarRef {
            name: VarName::new(path.clone()),
            subscripts: vec![],
        };
    }

    if let Some(index_expr) = component_ref_with_dynamic_final_subscripts(cr) {
        return index_expr;
    }

    Expression::from_component_ref(cr)
}

fn component_ref_with_dynamic_final_subscripts(cr: &ast::ComponentReference) -> Option<Expression> {
    let last = cr.parts.last()?;
    let last_subs_ast = last.subs.as_ref()?;
    if last_subs_ast.is_empty() {
        return None;
    }

    let last_subs: Vec<Subscript> = last_subs_ast.iter().map(Subscript::from_ast).collect();
    let has_dynamic = last_subs
        .iter()
        .any(|sub| !matches!(sub, Subscript::Index(_)));
    if !has_dynamic {
        return None;
    }

    let mut name_parts: Vec<String> = Vec::with_capacity(cr.parts.len());
    for (idx, part) in cr.parts.iter().enumerate() {
        let base = part.ident.text.to_string();
        if idx + 1 == cr.parts.len() {
            // Keep dynamic final subscripts as structured Index nodes.
            name_parts.push(base);
            continue;
        }

        let seg = match &part.subs {
            Some(subs) if !subs.is_empty() => {
                let sub_text: Vec<String> = subs.iter().map(subscript_to_string).collect();
                format!("{base}[{}]", sub_text.join(","))
            }
            _ => base,
        };
        name_parts.push(seg);
    }

    Some(Expression::Index {
        base: Box::new(Expression::VarRef {
            name: VarName::new(name_parts.join(".")),
            subscripts: vec![],
        }),
        subscripts: last_subs,
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

    canonical_path
        .rsplit('.')
        .next()
        .is_some_and(is_quoted_identifier)
}

/// Modelica quoted identifiers use single quotes (e.g., `'1'`).
fn is_quoted_identifier(name: &str) -> bool {
    name.starts_with('\'') && name.ends_with('\'') && name.len() >= 2
}
