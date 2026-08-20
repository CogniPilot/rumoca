//! Member identity proofs inside a selected redeclare target class.
//!
//! Once instantiation selects the concrete class for a replaceable alias, every
//! member segment of a deferred reference or of a class-override modifier is
//! re-proved against the selected class and its effective components.

use super::class_hierarchy::find_nested_class_in_hierarchy;
use crate::{InstantiateError, InstantiateResult};
use rumoca_core::DefId;
use rumoca_ir_ast as ast;

pub(super) fn resolve_member_reference_in_class(
    tree: &ast::ClassTree,
    selected_class_def_id: DefId,
    reference: &ast::ComponentReference,
    first_member: usize,
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
) -> InstantiateResult<Option<(DefId, Option<DefId>)>> {
    let effective_components = crate::get_effective_components(tree, owner_class)?;
    let Some(component) = effective_components.get(member_name) else {
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
        .then(|| {
            component.type_def_id.ok_or_else(|| {
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
                let identities =
                    resolve_member_reference_in_class(tree, selected_class_def_id, &target, 0)?;
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
