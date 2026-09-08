//! Application of the effective type selection to a component declaration.
//!
//! MLS §7.3 type redeclarations are applied by exact declaration identity, and
//! dotted type names rooted in a redeclared package are re-proved against the
//! selected package member. A same-named candidate that is neither a
//! redeclaration nor an extends relative of the resolved type is rejected so
//! unrelated lexical collisions cannot masquerade as an override.

use super::override_map::TypeOverrideMap;
use crate::type_lookup::find_member_type_in_class;
use crate::{InstantiateError, InstantiateResult};
use rumoca_core::DefId;
use rumoca_ir_ast as ast;

pub(super) fn is_predefined_identity(tree: &ast::ClassTree, def_id: DefId) -> bool {
    rumoca_core::BUILTIN_TYPES.iter().any(|name| {
        tree.scope_tree
            .predefined_member(&rumoca_core::ComponentPath::from_flat_path(name))
            == Some(def_id)
    })
}

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
    let exact_override = if let Some(source_def_id) = comp.type_def_id
        && let Some(target_def_id) = type_overrides.checked_target_for_alias_def_id(
            tree,
            source_def_id,
            comp.location.span(),
        )?
        && exact_type_override_preserves_declaration_slot(tree, source_def_id, target_def_id)?
    {
        Some(target_def_id)
    } else {
        None
    };
    let dynamic_root_override = (|| -> InstantiateResult<Option<DefId>> {
        if comp.type_def_id.is_some() || comp.type_name.name.len() < 2 {
            return Ok(None);
        }
        let dynamic_root_def_id = comp.type_name.def_id.ok_or_else(|| {
            Box::new(InstantiateError::missing_resolved_identity(
                format!("dynamic type root `{}`", comp.type_name),
                comp.location.span(),
            ))
        })?;
        let Some(selected_class_def_id) = type_overrides.checked_target_for_alias_def_id(
            tree,
            dynamic_root_def_id,
            comp.location.span(),
        )?
        else {
            return Ok(None);
        };
        let selected_class = tree
            .get_class_by_def_id(selected_class_def_id)
            .ok_or_else(|| {
                Box::new(InstantiateError::missing_resolved_identity(
                    format!("selected class {selected_class_def_id:?}"),
                    comp.location.span(),
                ))
            })?;
        let member_path = comp
            .type_name
            .name
            .iter()
            .skip(1)
            .map(|part| part.text.as_ref())
            .collect::<Vec<_>>();
        Ok(
            find_member_type_path_segments(tree, selected_class, &member_path)?
                .and_then(|member| member.def_id),
        )
    })()?;

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
) -> InstantiateResult<bool> {
    if source_def_id == target_def_id {
        if is_predefined_identity(tree, source_def_id) {
            return Ok(true);
        }
        tree.get_class_by_def_id(source_def_id).ok_or_else(|| {
            Box::new(InstantiateError::ModelNotFound(format!(
                "type override source {source_def_id:?}"
            )))
        })?;
        return Ok(true);
    }
    let source = tree.get_class_by_def_id(source_def_id).ok_or_else(|| {
        Box::new(InstantiateError::ModelNotFound(format!(
            "type override source {source_def_id:?}"
        )))
    })?;
    if is_predefined_identity(tree, target_def_id) {
        return Ok(true);
    }
    let target = tree.get_class_by_def_id(target_def_id).ok_or_else(|| {
        Box::new(InstantiateError::ModelNotFound(format!(
            "type override target {target_def_id:?}"
        )))
    })?;
    // Differently named targets are explicit class/package aliases. For
    // same-named declarations, require a structural redeclaration or extends
    // relationship so unrelated lexical collisions cannot masquerade as an
    // override.
    Ok(source.name.text != target.name.text
        || class_identity_reaches(tree, target, source_def_id)?
        || class_identity_reaches(tree, source, target_def_id)?)
}

fn class_identity_reaches(
    tree: &ast::ClassTree,
    root: &ast::ClassDef,
    target_def_id: DefId,
) -> InstantiateResult<bool> {
    let mut pending = vec![root];
    let mut complete = std::collections::HashSet::new();
    let mut reaches = false;

    while let Some(class) = pending.pop() {
        let def_id = class.def_id.ok_or_else(|| {
            Box::new(InstantiateError::missing_resolved_identity(
                format!("type selection class `{}`", class.name.text),
                class.location.span(),
            ))
        })?;
        if !complete.insert(def_id) {
            continue;
        }
        reaches |= def_id == target_def_id;
        if let Some(redeclare_target_def_id) = class.redeclare_target_def_id {
            let redeclare_target = tree
                .get_class_by_def_id(redeclare_target_def_id)
                .ok_or_else(|| {
                    Box::new(InstantiateError::missing_resolved_identity(
                        format!(
                            "redeclare target {redeclare_target_def_id:?} from `{}`",
                            class.name.text
                        ),
                        class.location.span(),
                    ))
                })?;
            pending.push(redeclare_target);
        }
        for extend in class.extends.iter().rev() {
            let base_name = extend.base_name.to_string();
            if crate::inheritance::predefined_extend_name(tree, extend)?.is_some() {
                continue;
            }
            let base_def_id = extend
                .base_def_id
                .or(extend.base_name.def_id)
                .ok_or_else(|| {
                    Box::new(InstantiateError::missing_resolved_identity(
                        format!("type selection extends edge `{base_name}`"),
                        extend.location.span(),
                    ))
                })?;
            let base = tree.get_class_by_def_id(base_def_id).ok_or_else(|| {
                Box::new(InstantiateError::missing_resolved_identity(
                    format!("type selection extends edge `{base_name}` ({base_def_id:?})"),
                    extend.location.span(),
                ))
            })?;
            pending.push(base);
        }
    }
    Ok(reaches)
}

fn find_member_type_path_segments<'a>(
    tree: &'a ast::ClassTree,
    class: &'a ast::ClassDef,
    member_path: &[&str],
) -> InstantiateResult<Option<&'a ast::ClassDef>> {
    let mut current = class;
    for segment in member_path {
        let Some(member) = find_member_type_in_class(tree, current, segment)? else {
            return Ok(None);
        };
        current = member;
    }
    Ok(Some(current))
}
