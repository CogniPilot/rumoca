//! Application of the effective type selection to a component declaration.
//!
//! MLS §7.3 type redeclarations are applied by exact declaration identity, and
//! dotted type names rooted in a redeclared package are re-proved against the
//! selected package member. A same-named candidate that is neither a
//! redeclaration nor an extends relative of the resolved type is rejected so
//! unrelated lexical collisions cannot masquerade as an override.

use super::class_hierarchy::extends_base_classes;
use super::override_map::TypeOverrideMap;
use crate::InstantiateResult;
use crate::type_lookup::find_member_type_in_class;
use rumoca_core::DefId;
use rumoca_ir_ast as ast;

/// Apply type override for replaceable type redeclarations (MLS §7.3).
pub(crate) fn apply_type_override<'a>(
    tree: &ast::ClassTree,
    comp: &'a ast::Component,
    type_overrides: &TypeOverrideMap,
) -> InstantiateResult<std::borrow::Cow<'a, ast::Component>> {
    // MLS §7.3: Apply type redeclarations by exact type name first.
    // For dotted type names (e.g., `Medium.ThermodynamicState`), also honor
    // package-level redeclarations keyed by the dotted prefix (`Medium`) when
    // the target member exists in the redeclared package.
    //
    // This must apply to package-member model types too (e.g.
    // `Medium.BaseProperties`), not only primitive/record members.
    let exact_override = comp.type_def_id.and_then(|source_def_id| {
        type_overrides
            .target_for_alias_def_id(source_def_id)
            .filter(|target_def_id| {
                exact_type_override_preserves_declaration_slot(tree, source_def_id, *target_def_id)
            })
    });
    let dynamic_root_override = (|| {
        if comp.type_def_id.is_some() || comp.type_name.name.len() < 2 {
            return None;
        }
        let dynamic_root_def_id = comp.type_name.def_id?;
        let selected_class_def_id = type_overrides.target_for_alias_def_id(dynamic_root_def_id)?;
        let selected_class = tree.get_class_by_def_id(selected_class_def_id)?;
        let member_path = comp
            .type_name
            .name
            .iter()
            .skip(1)
            .map(|part| part.text.as_ref())
            .collect::<Vec<_>>();
        find_member_type_path_segments(tree, selected_class, &member_path)?.def_id
    })();

    let override_def_id = dynamic_root_override.or(exact_override);
    if let Some(override_def_id) = override_def_id
        && comp.type_def_id != Some(override_def_id)
    {
        // Note: the MLS §7.3.2 constraining-type check happens in the
        // extends-redeclare path (`validate_redeclaration`); this override
        // map also carries package-member type remaps (Medium.X), which are
        // constrained at the package level and must not be re-checked here.
        let mut overridden = comp.clone();
        overridden.type_def_id = Some(override_def_id);
        return Ok(std::borrow::Cow::Owned(overridden));
    }
    Ok(std::borrow::Cow::Borrowed(comp))
}

fn exact_type_override_preserves_declaration_slot(
    tree: &ast::ClassTree,
    source_def_id: DefId,
    target_def_id: DefId,
) -> bool {
    if source_def_id == target_def_id {
        return true;
    }
    let (Some(source), Some(target)) = (
        tree.get_class_by_def_id(source_def_id),
        tree.get_class_by_def_id(target_def_id),
    ) else {
        return false;
    };

    // Differently named targets are explicit class/package aliases. For
    // same-named declarations, require a structural redeclaration or extends
    // relationship so unrelated lexical collisions cannot masquerade as an
    // override.
    source.name.text != target.name.text
        || class_identity_reaches(tree, target, source_def_id)
        || class_identity_reaches(tree, source, target_def_id)
}

fn class_identity_reaches(
    tree: &ast::ClassTree,
    root: &ast::ClassDef,
    target_def_id: DefId,
) -> bool {
    const MAX_DEPTH: usize = 32;
    let mut pending = vec![root];
    let mut visited = std::collections::HashSet::new();

    for _ in 0..MAX_DEPTH {
        let Some(class) = pending.pop() else {
            return false;
        };
        let Some(def_id) = class.def_id else {
            continue;
        };
        if def_id == target_def_id {
            return true;
        }
        if !visited.insert(def_id) {
            continue;
        }
        if let Some(redeclare_target) = class
            .redeclare_target_def_id
            .and_then(|def_id| tree.get_class_by_def_id(def_id))
        {
            pending.push(redeclare_target);
        }
        pending.extend(extends_base_classes(tree, class));
    }
    false
}

fn find_member_type_path_segments<'a>(
    tree: &'a ast::ClassTree,
    class: &'a ast::ClassDef,
    member_path: &[&str],
) -> Option<&'a ast::ClassDef> {
    let mut current = class;
    for segment in member_path {
        current = find_member_type_in_class(tree, current, segment)?;
    }
    Some(current)
}
