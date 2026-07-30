//! Exact restatement of a callable reference's structured path.
//!
//! A callable reference carries two spellings: the structured `ComponentRef`
//! parts (its use-site path, one exact `DefId` per segment) and the rendered
//! `VarName` (its lookup-qualified path, the key the collected function table
//! uses). This module closes the gap between them without inventing identity.

use rumoca_ir_ast as ast;

/// Restate `reference`'s structured path so it spells the rendered callable
/// name, when the rendered name is that same path qualified by the enclosing
/// scopes of its root segment.
///
/// Resolve records a callable's use-site spelling in the structured parts and
/// its lookup-qualified spelling (MLS §5.3, §13.2) as the rendered name, so a
/// reference written `Concrete.Element` inside `package P` renders
/// `P.Concrete.Element`. Both name the same declaration, but the rendered form
/// is the exposure identity the collected function table is keyed by, and only
/// the rendered form separates two exposures of one declaration
/// (`P.Generic.Element` versus `P.Concrete.Element`), so it cannot be dropped.
/// Restating the structured path closes the split without inventing anything:
/// every prepended segment is an exact enclosing-scope `DefId` taken from the
/// class index, and the restatement is admitted only when the rebuilt path
/// spells the rendered name exactly. A reference whose rendered name disagrees
/// for any other reason keeps its structured path and is left for the
/// callable-identity check to reject.
pub(super) fn scope_qualified_reference(
    class_index: &ast::ClassDefIndex<'_>,
    reference: &rumoca_core::Reference,
) -> Option<rumoca_core::Reference> {
    if reference.resolved_function().is_some() {
        return None;
    }
    let component_ref = reference.component_ref()?;
    let rendered = reference.var_name();
    if component_ref.to_var_name() == *rendered {
        return None;
    }
    let root = component_ref.parts().first()?;
    let mut enclosing = Vec::new();
    let mut current = class_index.parent_def_id(root.def_id);
    while let Some(def_id) = current {
        enclosing.push(def_id);
        current = class_index.parent_def_id(def_id);
    }
    // `enclosing` runs innermost-first; try every scope depth so a rendered
    // name that names only part of the chain still reconciles exactly.
    for depth in 1..=enclosing.len() {
        let mut parts = Vec::with_capacity(depth + component_ref.parts().len());
        for def_id in enclosing[..depth].iter().rev() {
            let scope = class_index.get(*def_id)?;
            parts.push(rumoca_core::ComponentRefPart {
                ident: scope.name.text.to_string(),
                span: root.span,
                subs: Vec::new(),
                def_id: *def_id,
            });
        }
        parts.extend(component_ref.parts().iter().cloned());
        let candidate = component_ref.with_replaced_parts(parts).ok()?;
        if candidate.to_var_name() == *rendered {
            return Some(
                reference.with_rewritten_component_reference(rendered.as_str(), candidate),
            );
        }
    }
    None
}
