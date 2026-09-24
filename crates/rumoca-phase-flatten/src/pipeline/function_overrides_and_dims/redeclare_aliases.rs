//! Redeclare aliases contributed by `extends Base(redeclare ... = X)`
//! modifications.

use super::*;

pub(super) fn collect_extends_redeclare_aliases_for_class(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    class_def: &rumoca_ir_ast::ClassDef,
    class_scope: &str,
    overrides: &mut OverrideEntries,
) {
    for ext in &class_def.extends {
        for modification in &ext.modifications {
            if !modification.redeclare {
                continue;
            }
            let Some((alias, value, slot_ref_def_id)) =
                redeclare_alias_and_value(&modification.expr)
            else {
                continue;
            };
            let Some(target_ref) = redeclare_value_type_ref(tree, class_index, class_scope, value)
            else {
                continue;
            };
            if is_receiver_alias_type(&target_ref.class_def.class_type) {
                let function_slot = extends_function_slot(class_index, ext, &alias, &target_ref);
                let active_redeclare = leaf_segment(&target_ref.name) != alias;
                let target = OverrideTarget::from_resolved_with_modifier_args(
                    alias.clone(),
                    target_ref,
                    active_redeclare,
                    redeclare_value_modifier_args(value),
                )
                .with_function_slot(function_slot);
                let alias_def_id = redeclare_alias_def_id(
                    class_index,
                    ext,
                    slot_ref_def_id,
                    &alias,
                    function_slot,
                );
                let target = match alias_def_id {
                    Some(alias_def_id) => target.with_alias_def_id(alias_def_id),
                    None => target,
                };
                overrides.insert_target(target);
            }
        }
    }
}

fn redeclare_alias_def_id(
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    ext: &rumoca_ir_ast::Extend,
    slot_ref_def_id: Option<rumoca_core::DefId>,
    alias: &str,
    function_slot: FunctionSlot,
) -> Option<rumoca_core::DefId> {
    match function_slot {
        FunctionSlot::Exact(alias_def_id) => Some(alias_def_id),
        FunctionSlot::Unrelated | FunctionSlot::Unresolved => {
            slot_ref_def_id.or_else(|| extends_alias_def_id(class_index, ext, alias))
        }
    }
}

/// Resolve the replaceable function declaration slot that an
/// `extends Base(redeclare F = X)` modification fills: the nested function
/// class named `alias` exposed by `Base`'s inheritance chain. Redeclares
/// whose target is not a function carry no function slot.
fn extends_function_slot(
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    ext: &rumoca_ir_ast::Extend,
    alias: &str,
    target_ref: &ResolvedClassRef<'_>,
) -> FunctionSlot {
    if target_ref.class_def.class_type != rumoca_core::ClassType::Function {
        return FunctionSlot::Unrelated;
    }
    let base_def_id = ext.base_def_id;
    let Some(base_def_id) = base_def_id else {
        return FunctionSlot::Unresolved;
    };
    match exact_package_function_exposure(
        class_index,
        base_def_id,
        alias,
        &mut FxHashSet::default(),
    ) {
        Ok(Some(slot_def_id)) => FunctionSlot::Exact(slot_def_id),
        Ok(None) | Err(_) => FunctionSlot::Unresolved,
    }
}

/// Collect element redeclares written in `class_def`'s own body
/// (`redeclare function F = X;`, MLS §7.3): nested function classes marked
/// `redeclare`, keyed by the inherited replaceable slot they replace.
pub(super) fn collect_element_redeclare_aliases_for_class(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    class_def: &rumoca_ir_ast::ClassDef,
    overrides: &mut OverrideEntries,
) {
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
        let function_slot = match nested.redeclare_target_def_id {
            Some(slot_def_id) => FunctionSlot::Exact(slot_def_id),
            None => FunctionSlot::Unresolved,
        };
        let target = OverrideTarget::from_resolved(alias.clone(), target_ref, true)
            .with_function_slot(function_slot);
        let target = match function_slot {
            FunctionSlot::Exact(alias_def_id) => target.with_alias_def_id(alias_def_id),
            FunctionSlot::Unresolved | FunctionSlot::Unrelated => target,
        };
        overrides.insert_target(target);
    }
}

fn extends_alias_def_id(
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    ext: &rumoca_ir_ast::Extend,
    alias: &str,
) -> Option<rumoca_core::DefId> {
    let base_def_id = ext.base_def_id?;
    let base = class_index.get(base_def_id)?;
    let mut visited = FxHashSet::default();
    let mut candidates = FxHashSet::default();
    collect_nested_alias_def_ids_into(class_index, base, alias, &mut visited, &mut candidates);
    let mut candidates = candidates.into_iter();
    let candidate = candidates.next()?;
    candidates.next().is_none().then_some(candidate)
}

fn collect_nested_alias_def_ids_into(
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    class_def: &rumoca_ir_ast::ClassDef,
    alias: &str,
    visited: &mut FxHashSet<rumoca_core::DefId>,
    candidates: &mut FxHashSet<rumoca_core::DefId>,
) {
    let Some(class_def_id) = class_def.def_id else {
        return;
    };
    if !visited.insert(class_def_id) {
        return;
    }
    if let Some(nested) = class_def.classes.get(alias)
        && let Some(def_id) = nested.def_id
    {
        candidates.insert(def_id);
    }
    for ext in &class_def.extends {
        let Some(base_def_id) = ext.base_def_id else {
            continue;
        };
        let Some(base) = class_index.get(base_def_id) else {
            continue;
        };
        collect_nested_alias_def_ids_into(class_index, base, alias, visited, candidates);
    }
}

/// Resolve `member` when one of `class_def`'s extends clauses redeclares it
/// (`extends Base(redeclare record Member = Target)`), returning the concrete
/// redeclare target class resolved in `class_scope`.
pub(crate) fn extends_class_redeclare_target<'a>(
    tree: &'a ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'a>,
    class_def: &rumoca_ir_ast::ClassDef,
    class_scope: &str,
    member: &str,
) -> Option<&'a rumoca_ir_ast::ClassDef> {
    for ext in &class_def.extends {
        for modification in &ext.modifications {
            if !modification.redeclare {
                continue;
            }
            let Some((alias, value, _)) = redeclare_alias_and_value(&modification.expr) else {
                continue;
            };
            if alias != member {
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
    &rumoca_ir_ast::Expression,
    Option<rumoca_core::DefId>,
)> {
    match expr {
        rumoca_ir_ast::Expression::Modification { target, value, .. } => Some((
            single_component_ref_name(target)?,
            value.as_ref(),
            target.target_def_id(),
        )),
        rumoca_ir_ast::Expression::Binary {
            op: rumoca_core::OpBinary::Assign,
            lhs,
            rhs,
            ..
        } => Some((
            redeclare_lhs_alias(lhs)?,
            rhs.as_ref(),
            redeclare_lhs_def_id(lhs),
        )),
        _ => None,
    }
}

fn redeclare_lhs_alias(expr: &rumoca_ir_ast::Expression) -> Option<String> {
    match expr {
        rumoca_ir_ast::Expression::ComponentReference(target) => single_component_ref_name(target),
        rumoca_ir_ast::Expression::ClassModification { target, .. } => {
            single_component_ref_name(target)
        }
        _ => None,
    }
}

fn redeclare_lhs_def_id(expr: &rumoca_ir_ast::Expression) -> Option<rumoca_core::DefId> {
    match expr {
        rumoca_ir_ast::Expression::ComponentReference(target)
        | rumoca_ir_ast::Expression::ClassModification { target, .. } => target.target_def_id(),
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
