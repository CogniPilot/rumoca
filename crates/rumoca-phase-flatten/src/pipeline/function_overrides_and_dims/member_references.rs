//! Member reference retargeting: rewriting package-member references
//! through the active override package.

use super::*;

pub(super) fn reference_source_package_def_id_from_index(
    reference: &rumoca_core::Reference,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) -> Option<rumoca_core::DefId> {
    if let Some(source_package_def_id) = reference
        .target_def_id()
        .and_then(|def_id| class_index.parent_def_id(def_id))
    {
        return Some(source_package_def_id);
    }
    let package_name = enclosing_scope(reference.as_str())?;
    class_index
        .get_by_qualified_name(package_name)
        .and_then(|class_def| class_def.def_id)
}

pub(super) fn resolve_override_member_name(
    reference: &rumoca_core::Reference,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> Option<String> {
    if reference_component_ref_is_instance_path(reference, ctx) {
        return None;
    }
    if let Some(resolved) = resolve_override_member_projection_name(reference, ctx) {
        return Some(resolved);
    }
    if reference_has_resolved_package_prefix(reference, ctx.class_index) {
        return None;
    }
    let scope = reference.component_scope()?;
    let member_leaf = scope.leaf_ident()?;
    let package = if let Some(source_package_def_id) = reference
        .target_def_id()
        .and_then(|def_id| ctx.class_index.parent_def_id(def_id))
    {
        ctx.active_override_package_for_source_package(source_package_def_id)?
    } else {
        ctx.unique_active_override_package()?
    };
    if let Some(scoped_name) =
        scoped_override_component_member_name(reference, package, &[member_leaf.to_string()], ctx)
    {
        return Some(scoped_name);
    }
    resolve_member_in_package_chain_exposed(ctx.tree, ctx.class_index, package, member_leaf)
        .filter(|resolved| resolved != reference.as_str())
}

/// Whether the reference already names a class/package-owned member.
///
/// A missing terminal `DefId` does not make a qualified reference relative:
/// `Modelica.Constants.eps`, for example, still carries a resolvable package
/// prefix. Such a reference cannot be captured by the only active replaceable
/// package merely because that package is unique. A path with no resolvable
/// prefix remains eligible for the relative-reference fallback below.
fn reference_has_resolved_package_prefix(
    reference: &rumoca_core::Reference,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) -> bool {
    let path = ComponentPath::from_reference(reference);
    (1..path.parts().len()).rev().any(|member_index| {
        path.prefix(member_index).is_some_and(|prefix| {
            class_index
                .get_by_qualified_name(&prefix.to_flat_string())
                .is_some()
        })
    })
}

fn resolve_override_member_projection_name(
    reference: &rumoca_core::Reference,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> Option<String> {
    let path = ComponentPath::from_reference(reference);
    let parts = path.parts();
    if parts.len() < 2 {
        return None;
    }

    for member_index in (1..parts.len()).rev() {
        let source_package = path.prefix(member_index)?.to_flat_string();
        let Some(source_package_def_id) = ctx
            .class_index
            .get_by_qualified_name(&source_package)
            .and_then(|class_def| class_def.def_id)
        else {
            continue;
        };
        let Some(package) = ctx.concrete_override_package_for_source_package(source_package_def_id)
        else {
            continue;
        };
        if let Some(scoped_name) =
            scoped_override_component_member_name(reference, package, &parts[member_index..], ctx)
        {
            return Some(scoped_name);
        }
        let member_leaf = parts[member_index].as_str();
        let Some(resolved_member) = resolve_member_in_package_chain_exposed(
            ctx.tree,
            ctx.class_index,
            package,
            member_leaf,
        ) else {
            continue;
        };
        let resolved = ComponentPath::from_flat_path(&resolved_member)
            .join_part_slice(&parts[member_index + 1..])
            .to_flat_string();
        if resolved != reference.as_str() {
            return Some(resolved);
        }
    }

    None
}

fn reference_component_ref_is_instance_path(
    reference: &rumoca_core::Reference,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> bool {
    canonical_instance_reference_name(reference, ctx).is_some()
}

pub(super) fn canonical_instance_reference_name(
    reference: &rumoca_core::Reference,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> Option<rumoca_core::Reference> {
    let component_ref = reference.component_ref()?;
    let component_path = ComponentPath::from_component_reference(component_ref);
    let component_name = component_path.to_flat_string();
    let is_known_instance_path = ctx
        .component_members
        .is_some_and(|scope| scope.contains_component_path(&component_path));
    ((is_known_instance_path || component_name != reference.as_str())
        && ctx
            .class_index
            .get_by_qualified_name(&component_name)
            .is_none()
        && enclosing_scope(&component_name)
            .is_none_or(|scope| ctx.class_index.get_by_qualified_name(scope).is_none()))
    .then(|| reference.with_var_name(rumoca_core::VarName::new(component_name)))
}
