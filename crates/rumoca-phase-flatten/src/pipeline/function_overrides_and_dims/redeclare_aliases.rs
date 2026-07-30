//! Redeclare aliases contributed by `extends Base(redeclare ... = X)`
//! modifications.

use super::*;

pub(super) fn collect_extends_redeclare_aliases_for_class(
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
    class_def: &rumoca_ir_ast::ClassDef,
    class_scope: &str,
    overrides: &mut rustc_hash::FxHashMap<String, OverrideTarget>,
) {
    for ext in &class_def.extends {
        for modification in &ext.modifications {
            if !modification.redeclare {
                continue;
            }
            let Some((alias, value)) = redeclare_alias_and_value(&modification.expr) else {
                continue;
            };
            let Some(target_ref) = redeclare_value_type_ref(tree, class_index, class_scope, value)
            else {
                continue;
            };
            if is_receiver_alias_type(&target_ref.class_def.class_type) {
                let active_redeclare = leaf_segment(&target_ref.name) != alias;
                overrides.insert(
                    alias.clone(),
                    OverrideTarget::from_resolved_with_modifier_args(
                        alias,
                        target_ref,
                        active_redeclare,
                        redeclare_value_modifier_args(value),
                    ),
                );
            }
        }
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
            let Some((alias, value)) = redeclare_alias_and_value(&modification.expr) else {
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
) -> Option<(String, &rumoca_ir_ast::Expression)> {
    match expr {
        rumoca_ir_ast::Expression::Modification { target, value, .. } => {
            Some((single_component_ref_name(target)?, value.as_ref()))
        }
        rumoca_ir_ast::Expression::Binary {
            op: rumoca_core::OpBinary::Assign,
            lhs,
            rhs,
            ..
        } => Some((redeclare_lhs_alias(lhs)?, rhs.as_ref())),
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
