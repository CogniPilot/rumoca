//! Effective virtual-class selections for one instantiation scope.
//!
//! [`TypeOverrideMap`] records which concrete class each replaceable class or
//! package alias currently selects (MLS §7.3). Selections are keyed both by the
//! alias declaration identity proved by Resolve and by the alias source path,
//! because an alias can be reached either way from a component declaration.

use super::class_hierarchy::extends_base_classes;
use super::redeclare_values::cref_to_qualified_name;
use crate::type_lookup::find_member_type_in_class;
use rumoca_core::{ClassType, DefId};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;
use rustc_hash::{FxHashMap, FxHashSet};

#[derive(Debug, Clone, Default)]
pub(crate) struct TypeOverrideMap {
    targets_by_alias_def_id: IndexMap<DefId, DefId>,
    targets_by_alias_path: IndexMap<ast::QualifiedName, DefId>,
    selected_member_targets: FxHashMap<(DefId, ast::QualifiedName), DefId>,
    rejected_selected_members: FxHashSet<(DefId, ast::QualifiedName)>,
}

impl TypeOverrideMap {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.targets_by_alias_def_id.is_empty()
            && self.targets_by_alias_path.is_empty()
            && self.selected_member_targets.is_empty()
            && self.rejected_selected_members.is_empty()
    }

    pub(crate) fn insert_alias(
        &mut self,
        alias_path: ast::QualifiedName,
        alias_def_id: Option<DefId>,
        target_def_id: DefId,
    ) {
        if let Some(alias_def_id) = alias_def_id {
            self.targets_by_alias_def_id
                .insert(alias_def_id, target_def_id);
        }
        self.targets_by_alias_path.insert(alias_path, target_def_id);
    }

    pub(super) fn insert_alias_if_absent(
        &mut self,
        alias_path: ast::QualifiedName,
        alias_def_id: Option<DefId>,
        target_def_id: DefId,
    ) {
        if let Some(alias_def_id) = alias_def_id {
            self.targets_by_alias_def_id
                .entry(alias_def_id)
                .or_insert(target_def_id);
        }
        self.targets_by_alias_path
            .entry(alias_path)
            .or_insert(target_def_id);
    }

    pub(crate) fn insert_class_override(&mut self, class_override: &ast::ClassOverride) {
        self.insert_alias(
            ast::QualifiedName::from_ident(&class_override.alias),
            Some(class_override.alias_def_id),
            class_override.target_def_id,
        );
    }

    pub(crate) fn extend_from(&mut self, other: &TypeOverrideMap) {
        for (alias_def_id, target_def_id) in &other.targets_by_alias_def_id {
            self.targets_by_alias_def_id
                .insert(*alias_def_id, *target_def_id);
        }
        for (alias_path, target_def_id) in &other.targets_by_alias_path {
            self.targets_by_alias_path
                .insert(alias_path.clone(), *target_def_id);
        }
        self.rejected_selected_members
            .extend(other.rejected_selected_members.iter().cloned());
        self.selected_member_targets.extend(
            other
                .selected_member_targets
                .iter()
                .map(|(key, value)| (key.clone(), *value)),
        );
    }

    pub(crate) fn target_for_alias_def_id(&self, alias_def_id: DefId) -> Option<DefId> {
        self.targets_by_alias_def_id.get(&alias_def_id).copied()
    }

    pub(crate) fn reject_selected_member(
        &mut self,
        selected_package_def_id: DefId,
        member_path: ast::QualifiedName,
    ) {
        self.rejected_selected_members
            .insert((selected_package_def_id, member_path));
    }

    pub(crate) fn selected_member_is_rejected(
        &self,
        selected_package_def_id: DefId,
        member_path: &ast::QualifiedName,
    ) -> bool {
        self.rejected_selected_members
            .contains(&(selected_package_def_id, member_path.clone()))
    }

    pub(crate) fn target_for_selected_member(
        &self,
        selected_package_def_id: DefId,
        member_path: &ast::QualifiedName,
    ) -> Option<DefId> {
        self.selected_member_targets
            .get(&(selected_package_def_id, member_path.clone()))
            .copied()
    }

    /// Index the exact nested member decisions for package selections already
    /// present in this map. The index is scoped by the selected package DefId;
    /// it never lets one package's same-named member overwrite another's.
    pub(crate) fn specialize_package_targets(&mut self, tree: &ast::ClassTree) {
        let mut package_targets = self
            .targets_by_alias_def_id
            .values()
            .copied()
            .filter(|target_def_id| {
                tree.get_class_by_def_id(*target_def_id)
                    .is_some_and(|class| class.class_type == ClassType::Package)
            })
            .collect::<Vec<_>>();
        package_targets.sort_unstable_by_key(|def_id| def_id.0);
        package_targets.dedup();
        for package_def_id in package_targets {
            self.specialize_inherited_nested_types_scoped(tree, package_def_id);
        }
    }

    pub(crate) fn target_for_alias_name(&self, alias: &str) -> Option<DefId> {
        self.targets_by_alias_path
            .get(&ast::QualifiedName::from_ident(alias))
            .copied()
    }

    pub(crate) fn target_for_reference(
        &self,
        reference: &ast::ComponentReference,
    ) -> Option<DefId> {
        let exact_def_id = (reference.parts.len() == 1)
            .then_some(reference.root_def_id())
            .flatten()
            .and_then(|def_id| self.target_for_alias_def_id(def_id));
        exact_def_id.or_else(|| {
            let path = cref_to_qualified_name(reference)?;
            self.targets_by_alias_path.get(&path).copied()
        })
    }

    /// Materialize the effective virtual-class selections for instance IR.
    ///
    /// Identity mappings describe locally declared classes, not overrides, so
    /// only changed declaration slots are retained.
    pub(crate) fn class_overrides(&self, tree: &ast::ClassTree) -> ast::ClassOverrideMap {
        self.targets_by_alias_def_id
            .iter()
            .filter_map(|(alias_def_id, target_def_id)| {
                if alias_def_id == target_def_id {
                    return None;
                }
                let alias = tree
                    .get_class_by_def_id(*alias_def_id)?
                    .name
                    .text
                    .to_string();
                Some((
                    *alias_def_id,
                    ast::ClassOverride::new(alias, *alias_def_id, *target_def_id, None),
                ))
            })
            .collect()
    }

    pub(super) fn target_for_path(&self, path: &ast::QualifiedName) -> Option<DefId> {
        self.targets_by_alias_path.get(path).copied()
    }

    /// Specialize inherited nested type identities for one effective package.
    ///
    /// A package alias such as `Medium` can select a derived package while the
    /// selected `BaseProperties` model is declared in an ancestor package.
    /// Components inherited by that model still carry the ancestor's resolved
    /// `DefId` (for example `PartialMedium.ThermodynamicState`). Once the
    /// package is selected for a concrete component instance, map every
    /// declaration of a nested member in its extends chain to the member that
    /// is effective in the selected package.
    pub(crate) fn specialize_inherited_nested_types(
        &mut self,
        tree: &ast::ClassTree,
        effective_package_def_id: DefId,
    ) {
        self.specialize_inherited_nested_types_internal(tree, effective_package_def_id, true);
    }

    fn specialize_inherited_nested_types_scoped(
        &mut self,
        tree: &ast::ClassTree,
        effective_package_def_id: DefId,
    ) {
        self.specialize_inherited_nested_types_internal(tree, effective_package_def_id, false);
    }

    fn specialize_inherited_nested_types_internal(
        &mut self,
        tree: &ast::ClassTree,
        effective_package_def_id: DefId,
        update_global_aliases: bool,
    ) {
        let Some(effective_package) = tree.get_class_by_def_id(effective_package_def_id) else {
            return;
        };
        let mut hierarchy = vec![(0usize, effective_package)];
        let mut visited = std::collections::HashSet::new();

        let mut index = 0;
        while index < hierarchy.len() {
            let (depth, class) = hierarchy[index];
            index += 1;
            if let Some(def_id) = class.def_id
                && !visited.insert(def_id)
            {
                continue;
            }
            hierarchy.extend(
                extends_base_classes(tree, class)
                    .into_iter()
                    .map(|base| (depth + 1, base)),
            );
        }

        let member_names: std::collections::BTreeSet<String> = hierarchy
            .iter()
            .flat_map(|(_, class)| class.classes.keys().cloned())
            .collect();
        for member_name in member_names {
            let Some(effective_member) =
                find_member_type_in_class(tree, effective_package, &member_name)
            else {
                continue;
            };
            let Some(effective_member_def_id) = effective_member.def_id else {
                continue;
            };
            let mut inherited_member_def_ids = hierarchy
                .iter()
                .filter_map(|(_, class)| {
                    class
                        .classes
                        .get(&member_name)
                        .and_then(|member| member.def_id)
                })
                .collect::<Vec<_>>();
            inherited_member_def_ids.sort_unstable_by_key(|def_id| def_id.0);
            inherited_member_def_ids.dedup();

            // An extends modifier may select an ordinary class as the RHS;
            // that class is not itself marked `redeclare`. The modifier's
            // resolved target slot and resolved RHS are the authoritative
            // identities in that form.
            let (has_authoritative_modifier, modifier_targets, rejected_slots) =
                collect_exact_modifier_targets(tree, &hierarchy, &inherited_member_def_ids);
            if has_authoritative_modifier {
                self.apply_authoritative_modifier_selection(
                    effective_package_def_id,
                    &member_name,
                    &inherited_member_def_ids,
                    modifier_targets,
                    rejected_slots,
                    update_global_aliases,
                );
                continue;
            }

            let Some(redeclare_target_def_id) = effective_member
                .is_redeclare
                .then_some(effective_member.redeclare_target_def_id)
                .flatten()
            else {
                continue;
            };
            let matching_slots = inherited_member_def_ids
                .into_iter()
                .filter(|slot| *slot == redeclare_target_def_id)
                .collect::<Vec<_>>();
            if let [matching_slot] = matching_slots.as_slice() {
                self.record_member_selection(
                    effective_package_def_id,
                    &member_name,
                    *matching_slot,
                    effective_member_def_id,
                    update_global_aliases,
                );
            }
        }
    }

    fn record_member_selection(
        &mut self,
        effective_package_def_id: DefId,
        member_name: &str,
        slot_def_id: DefId,
        target_def_id: DefId,
        update_global_aliases: bool,
    ) {
        self.selected_member_targets.insert(
            (
                effective_package_def_id,
                ast::QualifiedName::from_ident(member_name),
            ),
            target_def_id,
        );
        if update_global_aliases {
            self.insert_alias(
                ast::QualifiedName::from_ident(member_name),
                Some(slot_def_id),
                target_def_id,
            );
        }
    }

    fn apply_authoritative_modifier_selection(
        &mut self,
        effective_package_def_id: DefId,
        member_name: &str,
        inherited_member_def_ids: &[DefId],
        modifier_targets: Vec<(DefId, DefId)>,
        rejected_slots: Vec<DefId>,
        update_global_aliases: bool,
    ) {
        if update_global_aliases {
            for slot_def_id in &rejected_slots {
                self.targets_by_alias_def_id.shift_remove(slot_def_id);
            }
        }
        if !rejected_slots.is_empty() {
            self.reject_selected_member(
                effective_package_def_id,
                ast::QualifiedName::from_ident(member_name),
            );
        }
        if modifier_targets.is_empty() {
            return;
        }
        if inherited_member_def_ids.len() == 1 {
            if rejected_slots.is_empty() && modifier_targets.len() == 1 {
                self.selected_member_targets.insert(
                    (
                        effective_package_def_id,
                        ast::QualifiedName::from_ident(member_name),
                    ),
                    modifier_targets[0].1,
                );
            }
            if update_global_aliases && rejected_slots.is_empty() {
                self.targets_by_alias_path.insert(
                    ast::QualifiedName::from_ident(member_name),
                    modifier_targets[0].1,
                );
            }
        }
        if update_global_aliases {
            for (slot_def_id, target_def_id) in modifier_targets {
                self.targets_by_alias_def_id
                    .insert(slot_def_id, target_def_id);
            }
        }
    }
}

fn collect_exact_modifier_targets(
    tree: &ast::ClassTree,
    hierarchy: &[(usize, &ast::ClassDef)],
    inherited_member_def_ids: &[DefId],
) -> (bool, Vec<(DefId, DefId)>, Vec<DefId>) {
    let mut targets = Vec::<(DefId, Option<DefId>)>::new();
    let mut has_authoritative_modifier = false;
    let mut depth = 0;
    while hierarchy
        .iter()
        .any(|(class_depth, _)| *class_depth == depth)
    {
        let class_targets =
            collect_modifier_targets_at_depth(tree, hierarchy, depth, inherited_member_def_ids);
        has_authoritative_modifier |= !class_targets.is_empty();
        for (slot_def_id, target_def_id) in class_targets {
            if targets.iter().any(|(slot, _)| *slot == slot_def_id) {
                continue;
            }
            targets.push((slot_def_id, target_def_id));
        }
        depth += 1;
    }
    targets.sort_unstable_by_key(|(slot, _)| slot.0);
    let rejected_slots = targets
        .iter()
        .filter_map(|(slot, target)| target.is_none().then_some(*slot))
        .collect();
    (
        has_authoritative_modifier,
        targets
            .into_iter()
            .filter_map(|(slot, target)| target.map(|target| (slot, target)))
            .collect(),
        rejected_slots,
    )
}

fn collect_modifier_targets_at_depth(
    tree: &ast::ClassTree,
    hierarchy: &[(usize, &ast::ClassDef)],
    depth: usize,
    inherited_member_def_ids: &[DefId],
) -> Vec<(DefId, Option<DefId>)> {
    hierarchy
        .iter()
        .filter(|(class_depth, _)| *class_depth == depth)
        .flat_map(|(_, class)| {
            class
                .extends
                .iter()
                .flat_map(|extend| extend.modifications.iter())
                .filter_map(|modification| {
                    exact_modifier_target(tree, modification, inherited_member_def_ids)
                })
        })
        .fold(Vec::new(), |mut targets, (slot_def_id, target_def_id)| {
            merge_modifier_target(&mut targets, slot_def_id, target_def_id);
            targets
        })
}

fn exact_modifier_target(
    tree: &ast::ClassTree,
    modification: &ast::ExtendModification,
    inherited_member_def_ids: &[DefId],
) -> Option<(DefId, Option<DefId>)> {
    if !modification.redeclare {
        return None;
    }
    let ast::Expression::Modification { target, value, .. } = &modification.expr else {
        return None;
    };
    let slot_def_id = target.target_def_id()?;
    inherited_member_def_ids.contains(&slot_def_id).then_some((
        slot_def_id,
        super::redeclare_values::resolve_redeclare_value_def_id(tree, value.as_ref(), None),
    ))
}

fn merge_modifier_target(
    targets: &mut Vec<(DefId, Option<DefId>)>,
    slot_def_id: DefId,
    target_def_id: Option<DefId>,
) {
    if let Some((_, existing)) = targets.iter_mut().find(|(slot, _)| *slot == slot_def_id) {
        if existing.is_some() && *existing != target_def_id {
            *existing = None;
        }
    } else {
        targets.push((slot_def_id, target_def_id));
    }
}
