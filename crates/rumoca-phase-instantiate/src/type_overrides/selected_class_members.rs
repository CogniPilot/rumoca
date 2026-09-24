//! Member identity proofs inside a selected redeclare target class.
//!
//! Once instantiation selects the concrete class for a replaceable alias, every
//! member segment of a deferred reference or of a class-override modifier is
//! re-proved against the selected class and its effective components.

#[cfg(test)]
mod tests;

use super::class_hierarchy::find_nested_class_in_hierarchy;
use super::component_type_selection::apply_type_override;
use super::override_collection::build_type_override_map;
use super::override_map::TypeOverrideMap;
use crate::inheritance::{InheritanceCache, get_effective_components_with_cache};
use crate::{InstantiateError, InstantiateResult};
use rumoca_core::DefId;
use rumoca_ir_ast as ast;
use rustc_hash::FxHashMap;

#[derive(Default)]
pub(crate) struct MemberResolutionCache {
    inheritance: InheritanceCache,
    effective_components: FxHashMap<DefId, ast::AstIndexMap<String, ast::Component>>,
}

impl MemberResolutionCache {
    fn component(
        &mut self,
        tree: &ast::ClassTree,
        owner_class: &ast::ClassDef,
        member_name: &str,
    ) -> InstantiateResult<Option<ast::Component>> {
        let Some(owner_def_id) = owner_class.def_id else {
            return Ok(crate::get_effective_components(tree, owner_class)?
                .get(member_name)
                .cloned());
        };
        if !self.effective_components.contains_key(&owner_def_id) {
            let effective =
                get_effective_components_with_cache(tree, owner_class, &mut self.inheritance)?;
            self.effective_components.insert(owner_def_id, effective);
        }
        Ok(self
            .effective_components
            .get(&owner_def_id)
            .and_then(|components| components.get(member_name))
            .cloned())
    }
}

pub(super) fn resolve_member_reference_in_class(
    tree: &ast::ClassTree,
    selected_class_def_id: DefId,
    reference: &ast::ComponentReference,
    first_member: usize,
    active_overrides: Option<&TypeOverrideMap>,
) -> InstantiateResult<Vec<DefId>> {
    let mut cache = MemberResolutionCache::default();
    resolve_member_reference_in_class_with_cache(
        tree,
        selected_class_def_id,
        reference,
        first_member,
        active_overrides,
        &mut cache,
    )
}

pub(crate) fn resolve_member_reference_in_class_with_cache(
    tree: &ast::ClassTree,
    selected_class_def_id: DefId,
    reference: &ast::ComponentReference,
    first_member: usize,
    active_overrides: Option<&TypeOverrideMap>,
    cache: &mut MemberResolutionCache,
) -> InstantiateResult<Vec<DefId>> {
    let root = reference.parts.first().ok_or_else(|| {
        Box::new(InstantiateError::redeclare_error(
            "<empty>",
            "deferred reference has no root",
            reference.span,
        ))
    })?;
    let mut owner_class_def_id = selected_class_def_id;
    let mut identities = Vec::with_capacity(reference.parts.len().saturating_sub(first_member));
    for (index, part) in reference.parts.iter().enumerate().skip(first_member) {
        let owner_class = tree
            .get_class_by_def_id(owner_class_def_id)
            .ok_or_else(|| {
                Box::new(InstantiateError::redeclare_error(
                    root.ident.text.as_ref(),
                    "selected redeclare class is absent from the resolved class tree",
                    reference.span,
                ))
            })?;
        if let Some((component_def_id, next_owner_def_id)) = resolve_component_member_step(
            tree,
            owner_class,
            part.ident.text.as_ref(),
            index + 1 < reference.parts.len(),
            reference.span,
            active_overrides,
            cache,
        )? {
            identities.push(component_def_id);
            if let Some(next_owner_def_id) = next_owner_def_id {
                owner_class_def_id = next_owner_def_id;
            }
            continue;
        }
        let nested = find_nested_class_in_hierarchy(tree, owner_class, part.ident.text.as_ref())
            .ok_or_else(|| {
                Box::new(InstantiateError::redeclare_error(
                    part.ident.text.as_ref(),
                    "selected redeclare class has no such member",
                    reference.span,
                ))
            })?;
        let target_def_id = nested.def_id.ok_or_else(|| {
            Box::new(InstantiateError::redeclare_error(
                part.ident.text.as_ref(),
                "effective nested redeclare member has no declaration identity",
                reference.span,
            ))
        })?;
        identities.push(target_def_id);
        owner_class_def_id = target_def_id;
    }
    Ok(identities)
}

fn resolve_component_member_step(
    tree: &ast::ClassTree,
    owner_class: &ast::ClassDef,
    member_name: &str,
    has_tail: bool,
    span: rumoca_core::Span,
    active_overrides: Option<&TypeOverrideMap>,
    cache: &mut MemberResolutionCache,
) -> InstantiateResult<Option<(DefId, Option<DefId>)>> {
    let Some(component) = cache.component(tree, owner_class, member_name)? else {
        return Ok(None);
    };
    let component_def_id = component.def_id.ok_or_else(|| {
        Box::new(InstantiateError::redeclare_error(
            member_name,
            "effective redeclare member has no declaration identity",
            span,
        ))
    })?;
    let next_owner_def_id = has_tail
        .then(|| -> InstantiateResult<DefId> {
            // Rebuild the lexical owner map first, then overlay the active
            // instance selections. The latter is required when a selected
            // class contains a dotted component such as Medium.BaseProperties:
            // Resolve intentionally leaves its type_def_id absent, while the
            // package anchor and the instance redeclare are authoritative.
            let mut overrides = build_type_override_map(tree, owner_class, None);
            if let Some(owner_def_id) = owner_class.def_id {
                overrides.specialize_inherited_nested_types(tree, owner_def_id);
            }
            if let Some(active_overrides) = active_overrides {
                overrides.extend_from(active_overrides);
            }
            if let Some(source_def_id) = component.type_def_id {
                return Ok(overrides
                    .target_for_alias_def_id(source_def_id)
                    .unwrap_or(source_def_id));
            }
            apply_type_override(tree, &component, &overrides)?
                .type_def_id
                .ok_or_else(|| {
                    Box::new(InstantiateError::redeclare_error(
                        member_name,
                        "intermediate redeclare member has no resolved class identity",
                        span,
                    ))
                })
        })
        .transpose()?;
    Ok(Some((component_def_id, next_owner_def_id)))
}

pub(crate) fn resolve_class_override_modifier_targets(
    tree: &ast::ClassTree,
    selected_class_def_id: DefId,
    modifiers: Vec<ast::Expression>,
) -> InstantiateResult<Vec<ast::Expression>> {
    modifiers
        .into_iter()
        .map(|modifier| match modifier {
            ast::Expression::Modification {
                mut target,
                value,
                span,
            } => {
                let identities = resolve_member_reference_in_class(
                    tree,
                    selected_class_def_id,
                    &target,
                    0,
                    None,
                )?;
                for (part, def_id) in target.parts.iter_mut().zip(identities) {
                    part.def_id = Some(def_id);
                }
                Ok(ast::Expression::Modification {
                    target,
                    value,
                    span,
                })
            }
            other => Ok(other),
        })
        .collect()
}
