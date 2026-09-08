//! Lookups that leave this scope and read a class declaration (MLS §5.3.2, §7.1).
//!
//! A parameter expression may name something the enclosing scope does not
//! declare: a constant belonging to a package, or a field belonging to the record
//! type of one of its components. Both are resolved here by following the
//! reference to the declaration that actually defines it.

use super::*;

/// Result of class/package-constant selection from resolved reference identity.
pub(super) enum ClassConstantResolution<'tree> {
    /// The root is not a resolved class, so this is a different reference form.
    NotClassReference,
    /// The root is a resolved class, but the exact target cannot denote an
    /// unsubscripted constant declaration.
    Refused,
    /// Exact effective exposure selected by the reference's class prefix,
    /// together with the inherited declaration identity to select inside it.
    Bound {
        exposure: &'tree ast::ClassDef,
        target: rumoca_core::DefId,
    },
}

/// Resolve a class/package constant through the declaration identity carried
/// by the reference, returning a closed selection rather than conflating an
/// invalid resolved target with an unresolved textual reference.
///
/// The exposure class is part of the result because inherited modifications
/// specialize the declaration there. Consumers select the target `DefId`
/// inside that effective class and evaluate its binding in the same scope.
pub(super) fn resolve_class_constant_route<'tree>(
    comp_ref: &ast::ComponentReference,
    index: &ast::ClassDefIndex<'tree>,
) -> ClassConstantResolution<'tree> {
    let Some(root) = comp_ref.root_def_id() else {
        return ClassConstantResolution::NotClassReference;
    };
    if index.get(root).is_none() {
        return ClassConstantResolution::NotClassReference;
    }
    if comp_ref
        .parts
        .iter()
        .any(|part| part.subs.as_ref().is_some_and(|subs| !subs.is_empty()))
    {
        return ClassConstantResolution::Refused;
    }
    let Some(target) = comp_ref.target_def_id() else {
        return ClassConstantResolution::Refused;
    };
    let Some(component) = index.component(target) else {
        return ClassConstantResolution::Refused;
    };
    if !matches!(component.variability, rumoca_core::Variability::Constant(_)) {
        return ClassConstantResolution::Refused;
    }
    let Some(exposure) = comp_ref
        .parts
        .get(comp_ref.parts.len().saturating_sub(2))
        .and_then(|part| part.def_id)
        .and_then(|exposure| index.get(exposure))
    else {
        return ClassConstantResolution::Refused;
    };
    ClassConstantResolution::Bound { exposure, target }
}

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
    tree.get_class_by_def_id(comp_ref.root_def_id()?)?;
    let exposure = comp_ref
        .parts
        .get(comp_ref.parts.len().checked_sub(2)?)?
        .def_id
        .and_then(|exposure| tree.get_class_by_def_id(exposure))?;
    let target = comp_ref.target_def_id()?;
    let effective_components = resolve_class_components(tree, exposure);
    let component = effective_components
        .values()
        .find(|component| component.def_id == Some(target))?;
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
    mod_env: &ast::ModificationEnvironment,
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
    let first: &str = comp_ref.parts.first()?.ident.text.as_ref();
    let mut current = effective_components.get(first)?.clone();
    let root_eval_ctx = InstantiateEvalCtx {
        tree,
        mod_env,
        effective_components,
        resolve_class_components,
    };
    if matches!(
        current.variability,
        rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
    ) && !component_allows_structural_evaluation(first, &current, &root_eval_ctx)
    {
        return None;
    }
    let mut expr = None;
    let mut occurrence_path = first.to_string();

    let field_parts = &comp_ref.parts[1..];
    for (index, field_part) in field_parts.iter().enumerate() {
        let field_name = field_part.ident.text.as_ref();
        let type_def_id = current.type_def_id?;
        let class = tree.get_class_by_def_id(type_def_id)?;
        let field_components = resolve_class_components(tree, class);
        let field_comp = field_components.get(field_name)?.clone();
        occurrence_path.push('.');
        occurrence_path.push_str(field_name);
        let is_target = index + 1 == field_parts.len();
        if (is_target
            || matches!(
                field_comp.variability,
                rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
            ))
            && !component_allows_structural_evaluation(
                &occurrence_path,
                &field_comp,
                &InstantiateEvalCtx {
                    tree,
                    mod_env,
                    effective_components: &field_components,
                    resolve_class_components,
                },
            )
        {
            return None;
        }
        expr = current
            .modifications
            .get(field_name)
            .cloned()
            .or_else(|| component_expr_for_structural_eval(&field_comp).cloned());
        current = field_comp;
    }

    expr
}
