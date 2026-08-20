//! Lookups that leave this scope and read a class declaration (MLS §5.3.2, §7.1).
//!
//! A parameter expression may name something the enclosing scope does not
//! declare: a constant belonging to a package, or a field belonging to the record
//! type of one of its components. Both are resolved here by following the
//! reference to the declaration that actually defines it.

use super::*;

/// Resolve a qualified reference like `P.pT_explicit` to the binding of a
/// class-level constant. Enclosing-scope constants are qualified to their
/// declaring class by the package-constant alias pass (MLS §5.3.2), so this
/// is the evaluation counterpart of that lexical lookup.
pub(super) fn resolve_class_constant_binding(
    comp_ref: &ast::ComponentReference,
    tree: &ast::ClassTree,
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> IndexMap<String, ast::Component>,
) -> Option<ast::Expression> {
    if comp_ref.parts.len() < 2
        || comp_ref
            .parts
            .iter()
            .any(|part| part.subs.as_ref().is_some_and(|subs| !subs.is_empty()))
    {
        return None;
    }
    let member = comp_ref.parts.last()?.ident.text.as_ref();
    let class_path = comp_ref.parts[..comp_ref.parts.len() - 1]
        .iter()
        .map(|part| part.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");
    let class = tree.get_class_by_qualified_name(&class_path)?;
    let effective_components = resolve_class_components(tree, class);
    let component = effective_components.get(member)?;
    if !matches!(component.variability, rumoca_core::Variability::Constant(_)) {
        return None;
    }
    component.binding.clone()
}

/// Resolve `record.field` to the value the record declares for that field.
///
/// MLS §7.1: a record's elements include the ones it inherits, so the field is
/// looked up in the class's *effective* components. `IM_SquirrelCageData`
/// declares no `ratioCommonStatorLeakage` of its own — it extends
/// `InductionMachineData`, which does — and a lookup restricted to the class's
/// own declarations would report the field as absent.
pub(super) fn resolve_component_ref_from_record_defaults(
    comp_ref: &ast::ComponentReference,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> IndexMap<String, ast::Component>,
) -> Option<ast::Expression> {
    if comp_ref.parts.len() < 2 || comp_ref.parts.iter().any(|part| part.subs.is_some()) {
        return None;
    }
    let mut parts = comp_ref.parts.iter().map(|part| part.ident.text.as_ref());
    let first: &str = parts.next()?;
    let mut current = effective_components.get(first)?.clone();
    let mut expr = None;

    for field_name in parts {
        if let Some(mod_expr) = current.modifications.get(field_name) {
            expr = Some(mod_expr.clone());
            break;
        }

        let type_def_id = current.type_def_id?;
        let class = tree.get_class_by_def_id(type_def_id)?;
        let field_comp = resolve_class_components(tree, class)
            .get(field_name)?
            .clone();
        expr = component_expr_for_structural_eval(&field_comp).cloned();
        current = field_comp;
    }

    expr
}
