use super::*;

/// Keep package-member values under the concrete component instance.
///
/// A replaceable package can modify an inherited constant independently for
/// every containing component (MLS §7.2/§7.3). Canonicalizing `Medium.k` to the
/// declaration name `Base.k` loses that instance identity and can select a
/// global/default value. The component constant injector materializes the
/// selected package as `<instance>.<alias>.<member>`, so value references must
/// use that same spelling. Function/class references continue through the
/// canonical override path in the parent module.
pub(super) fn scoped_override_component_member_name(
    reference: &rumoca_core::Reference,
    package: &OverrideTarget,
    member_and_suffix: &[String],
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> Option<String> {
    reference.target_def_id()?;
    if ctx.active_scope.is_root() || package.alias.is_empty() || member_and_suffix.is_empty() {
        return None;
    }
    let mut relative = Vec::with_capacity(member_and_suffix.len() + 1);
    relative.push(package.alias.clone());
    relative.extend(member_and_suffix.iter().cloned());
    Some(ctx.active_scope.join_part_slice(&relative).to_flat_string())
}
