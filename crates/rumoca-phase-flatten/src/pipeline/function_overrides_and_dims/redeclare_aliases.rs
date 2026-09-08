//! Redeclare aliases contributed by `extends Base(redeclare ... = X)`
//! modifications.

use super::*;

pub(super) fn collect_extends_redeclare_aliases_for_class(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    class_def: &rumoca_ir_ast::ClassDef,
    class_scope: &str,
    overrides: &mut AliasOverrideTable,
) -> Result<(), FlattenError> {
    for ext in &class_def.extends {
        for modification in &ext.modifications {
            if !modification.redeclare {
                continue;
            }
            let Some((alias, alias_target_def_id, value)) =
                redeclare_alias_and_value(&modification.expr)
            else {
                continue;
            };
            let Some(target_ref) = redeclare_value_type_ref(tree, class_index, class_scope, value)
            else {
                continue;
            };
            if !is_receiver_alias_type(&target_ref.class_def.class_type) {
                continue;
            }
            let Some(alias_slot) = alias_target_def_id
                .or_else(|| extends_alias_slot(class_index, class_scope, ext, &alias))
            else {
                return Err(FlattenError::unhonored_function_redeclare(
                    &alias,
                    format!(
                        "the extends redeclare `{alias}` names no exact replaceable slot identity"
                    ),
                    required_location_span(&tree.source_map, &ext.location, "extends redeclare")?,
                ));
            };
            let function_slot =
                if target_ref.class_def.class_type == rumoca_core::ClassType::Function {
                    FunctionSlot::Exact(alias_slot)
                } else {
                    FunctionSlot::Unrelated
                };
            let default_selection = resolve_package_alias_chain(tree, class_index, alias_slot)
                .map(|default_ref| default_ref.def_id);
            let selected = resolve_package_alias_chain(tree, class_index, target_ref.def_id)
                .map(|selected_ref| selected_ref.def_id)
                .unwrap_or(target_ref.def_id);
            let active_redeclare = default_selection != Some(selected);
            overrides.insert(
                alias_slot,
                OverrideTarget::from_resolved_with_modifier_args(
                    alias,
                    alias_slot,
                    target_ref,
                    active_redeclare,
                    redeclare_value_modifier_args(value),
                )
                .with_function_slot(function_slot),
            );
        }
    }
    Ok(())
}

/// Resolve the replaceable declaration slot that an
/// `extends Base(redeclare M = X)` modification fills: the nested class
/// named `alias` exposed by `Base`'s inheritance chain. The name binding
/// happens once, against the base the extends clause itself names; every
/// later decision uses the returned identity.
fn extends_alias_slot(
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    class_scope: &str,
    ext: &rumoca_ir_ast::Extend,
    alias: &str,
) -> Option<rumoca_core::DefId> {
    let base_def_id = ext.base_def_id.or(ext.base_name.def_id).or_else(|| {
        resolve_class_in_scope_indexed(class_index, &ext.base_name.to_string(), class_scope)
            .0
            .and_then(|class_def| class_def.def_id)
    })?;
    exact_package_member_class_slot(class_index, base_def_id, alias, &mut FxHashSet::default())
}

/// The nested class declaration named `member` exposed by `package`'s
/// inheritance chain, by exact `DefId`.
fn exact_package_member_class_slot(
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    package: rumoca_core::DefId,
    member: &str,
    visited: &mut FxHashSet<rumoca_core::DefId>,
) -> Option<rumoca_core::DefId> {
    if !visited.insert(package) {
        return None;
    }
    let class_def = class_index.get(package)?;
    if let Some(nested) = class_def.classes.get(member) {
        return nested.def_id;
    }
    for extend in &class_def.extends {
        let Some(base) = extend.base_def_id.or(extend.base_name.def_id) else {
            continue;
        };
        if let Some(slot) = exact_package_member_class_slot(class_index, base, member, visited) {
            return Some(slot);
        }
    }
    None
}

/// Collect element redeclares written in `class_def`'s own body
/// (`redeclare function F = X;`, MLS §7.3): nested function classes marked
/// `redeclare`, keyed by the inherited replaceable slot they replace.
pub(super) fn collect_element_redeclare_aliases_for_class(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    class_def: &rumoca_ir_ast::ClassDef,
    overrides: &mut AliasOverrideTable,
) -> Result<(), FlattenError> {
    for (alias, nested) in &class_def.classes {
        if !nested.is_redeclare || nested.class_type != rumoca_core::ClassType::Function {
            continue;
        }
        let Some(nested_def_id) = nested.def_id else {
            continue;
        };
        let Some(target_ref) = resolved_class_ref_for_def_id(tree, class_index, nested_def_id)
        else {
            continue;
        };
        let Some(slot_def_id) = nested.redeclare_target_def_id else {
            return Err(FlattenError::unhonored_function_redeclare(
                alias,
                format!(
                    "the function redeclare `{alias}` names no exact replaceable slot identity"
                ),
                required_location_span(&tree.source_map, &nested.location, "function redeclare")?,
            ));
        };
        overrides.insert(
            slot_def_id,
            OverrideTarget::from_resolved(alias.clone(), slot_def_id, target_ref, true)
                .with_function_slot(FunctionSlot::Exact(slot_def_id)),
        );
    }
    Ok(())
}

/// Resolve `member` when one of `class_def`'s extends clauses redeclares it
/// (`extends Base(redeclare record Member = Target)`), returning the concrete
/// redeclare target class resolved in `class_scope`.
///
/// The queried member is bound to its slot identity in the extends base
/// first; a modification matches by that identity when both sides carry one,
/// so an inner redeclare is never matched by an outer same-spelled alias.
pub(crate) fn extends_class_redeclare_target<'a>(
    tree: &'a ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'a>,
    class_def: &rumoca_ir_ast::ClassDef,
    class_scope: &str,
    member: &str,
) -> Option<&'a rumoca_ir_ast::ClassDef> {
    for ext in &class_def.extends {
        let member_slot = extends_alias_slot(class_index, class_scope, ext, member);
        for modification in &ext.modifications {
            if !modification.redeclare {
                continue;
            }
            let Some((alias, alias_target_def_id, value)) =
                redeclare_alias_and_value(&modification.expr)
            else {
                continue;
            };
            let matches_member = match (alias_target_def_id, member_slot) {
                (Some(alias_slot), Some(member_slot)) => alias_slot == member_slot,
                _ => alias == member,
            };
            if !matches_member {
                continue;
            }
            let target = redeclare_value_type_ref(tree, class_index, class_scope, value)?;
            return Some(target.class_def);
        }
    }
    None
}

fn redeclare_alias_and_value(
    expr: &rumoca_ir_ast::Expression,
) -> Option<(
    String,
    Option<rumoca_core::DefId>,
    &rumoca_ir_ast::Expression,
)> {
    match expr {
        rumoca_ir_ast::Expression::Modification {
            target,
            value: Some(value),
            ..
        } => Some((
            single_component_ref_name(target)?,
            target.target_def_id(),
            value.as_ref(),
        )),
        rumoca_ir_ast::Expression::Binary {
            op: rumoca_core::OpBinary::Assign,
            lhs,
            rhs,
            ..
        } => {
            let (alias, alias_target_def_id) = redeclare_lhs_alias(lhs)?;
            Some((alias, alias_target_def_id, rhs.as_ref()))
        }
        _ => None,
    }
}

fn redeclare_lhs_alias(
    expr: &rumoca_ir_ast::Expression,
) -> Option<(String, Option<rumoca_core::DefId>)> {
    match expr {
        rumoca_ir_ast::Expression::ComponentReference(target)
        | rumoca_ir_ast::Expression::ClassModification { target, .. } => {
            Some((single_component_ref_name(target)?, target.target_def_id()))
        }
        _ => None,
    }
}

fn redeclare_value_type_ref<'a>(
    tree: &'a ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'a>,
    class_scope: &str,
    value: &rumoca_ir_ast::Expression,
) -> Option<ResolvedClassRef<'a>> {
    let cref = match value {
        rumoca_ir_ast::Expression::ComponentReference(cref) => cref,
        rumoca_ir_ast::Expression::FunctionCall { comp, .. } => comp,
        rumoca_ir_ast::Expression::ClassModification { target, .. } => target,
        _ => return None,
    };
    // The redeclare value names a class, so the class is the reference's exact
    // *target* segment. `root_def_id` is the first segment, which for a dotted
    // class reference such as `Modelica.Blocks.Sources.Step` identifies the
    // enclosing package `Modelica`, not the redeclared class (MLS §5.3, §7.3).
    if let Some(def_id) = cref.target_def_id() {
        return Some(ResolvedClassRef {
            name: tree.def_map.get(&def_id)?.clone(),
            def_id,
            class_def: class_index.get(def_id)?,
        });
    }
    let name = resolve_class_ref_name(tree, cref).or_else(|| {
        resolve_class_in_scope_indexed(class_index, &cref.to_string(), class_scope).1
    })?;
    let class_def = class_index.get_by_qualified_name(&name)?;
    Some(ResolvedClassRef {
        def_id: class_def.def_id?,
        class_def,
        name,
    })
}

fn redeclare_value_modifier_args(value: &rumoca_ir_ast::Expression) -> Vec<FunctionModifierArg> {
    let args = match value {
        rumoca_ir_ast::Expression::FunctionCall { args, .. } => args,
        rumoca_ir_ast::Expression::ClassModification { modifications, .. } => modifications,
        _ => return Vec::new(),
    };
    args.iter()
        .filter_map(function_modifier_arg_from_ast)
        .collect()
}
