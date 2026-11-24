//! Effective virtual-class selections for one instantiation scope.
//!
//! [`TypeOverrideMap`] records which concrete class each replaceable class or
//! package alias currently selects (MLS §7.3). Selections are keyed both by the
//! alias declaration identity proved by Resolve and by the alias source path,
//! because an alias can be reached either way from a component declaration.

use super::class_hierarchy::extends_base_classes;
use crate::type_lookup::find_member_type_in_class;
use rumoca_core::DefId;
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;
use std::sync::Arc;

#[derive(Debug, Clone, Default)]
pub(crate) struct ClassModifierValues {
    pub(crate) bindings: IndexMap<ast::QualifiedName, ast::ModificationValue>,
    pub(crate) final_paths: Vec<ast::QualifiedName>,
}

#[derive(Debug, Clone)]
struct TypeSelection {
    target_def_id: DefId,
    modifier_values: Option<Arc<ClassModifierValues>>,
}

impl TypeSelection {
    fn new(target_def_id: DefId) -> Self {
        Self {
            target_def_id,
            modifier_values: None,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub(crate) struct TypeOverrideMap {
    targets_by_alias_def_id: IndexMap<DefId, TypeSelection>,
    targets_by_alias_path: IndexMap<ast::QualifiedName, DefId>,
}

impl TypeOverrideMap {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.targets_by_alias_def_id.is_empty() && self.targets_by_alias_path.is_empty()
    }

    pub(crate) fn insert_alias(
        &mut self,
        alias_path: ast::QualifiedName,
        alias_def_id: Option<DefId>,
        target_def_id: DefId,
    ) {
        if let Some(alias_def_id) = alias_def_id {
            self.targets_by_alias_def_id
                .insert(alias_def_id, TypeSelection::new(target_def_id));
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
                .or_insert_with(|| TypeSelection::new(target_def_id));
        }
        self.targets_by_alias_path
            .entry(alias_path)
            .or_insert(target_def_id);
    }

    pub(crate) fn insert_class_override(&mut self, class_override: &ast::ClassOverride) {
        self.insert_class_override_with_values(class_override, ClassModifierValues::default());
    }

    pub(crate) fn insert_class_override_with_values(
        &mut self,
        class_override: &ast::ClassOverride,
        values: ClassModifierValues,
    ) {
        let modifier_values = (!values.bindings.is_empty() || !values.final_paths.is_empty())
            .then(|| Arc::new(values));
        self.targets_by_alias_def_id.insert(
            class_override.alias_def_id,
            TypeSelection {
                target_def_id: class_override.target_def_id,
                modifier_values,
            },
        );
        self.targets_by_alias_path.insert(
            ast::QualifiedName::from_ident(&class_override.alias),
            class_override.target_def_id,
        );
    }

    pub(crate) fn extend_from(&mut self, other: &TypeOverrideMap) {
        for (alias_def_id, selection) in &other.targets_by_alias_def_id {
            self.targets_by_alias_def_id
                .insert(*alias_def_id, selection.clone());
        }
        for (alias_path, target_def_id) in &other.targets_by_alias_path {
            self.targets_by_alias_path
                .insert(alias_path.clone(), *target_def_id);
        }
    }

    pub(crate) fn target_for_alias_def_id(&self, alias_def_id: DefId) -> Option<DefId> {
        self.targets_by_alias_def_id
            .get(&alias_def_id)
            .map(|selection| selection.target_def_id)
    }

    pub(crate) fn class_modifier_values(
        &self,
        alias_def_id: DefId,
    ) -> Option<&ClassModifierValues> {
        self.targets_by_alias_def_id
            .get(&alias_def_id)?
            .modifier_values
            .as_deref()
    }

    pub(crate) fn target_for_alias_name(&self, alias: &str) -> Option<DefId> {
        self.targets_by_alias_path
            .get(&ast::QualifiedName::from_ident(alias))
            .copied()
    }

    pub(super) fn selected_redeclare_value(&self, mut rhs: DefId) -> DefId {
        let mut visited = rustc_hash::FxHashSet::default();
        while let Some(selected) = self.target_for_alias_def_id(rhs)
            && rhs != selected
            && visited.insert(rhs)
        {
            rhs = selected;
        }
        rhs
    }

    /// Materialize the effective virtual-class selections for instance IR.
    ///
    /// Identity mappings describe locally declared classes, not overrides, so
    /// only changed declaration slots are retained.
    pub(crate) fn class_overrides(&self, tree: &ast::ClassTree) -> ast::ClassOverrideMap {
        self.targets_by_alias_def_id
            .iter()
            .filter_map(|(alias_def_id, selection)| {
                if *alias_def_id == selection.target_def_id {
                    return None;
                }
                let alias = tree
                    .get_class_by_def_id(*alias_def_id)?
                    .name
                    .text
                    .to_string();
                Some((
                    *alias_def_id,
                    ast::ClassOverride::new(alias, *alias_def_id, selection.target_def_id, None),
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
        let Some(effective_package) = tree.get_class_by_def_id(effective_package_def_id) else {
            return;
        };
        let mut hierarchy = vec![effective_package];
        let mut visited = std::collections::HashSet::new();

        for index in 0..hierarchy.len() {
            let class = hierarchy[index];
            if let Some(def_id) = class.def_id
                && !visited.insert(def_id)
            {
                continue;
            }
            hierarchy.extend(extends_base_classes(tree, class));
        }

        let member_names: std::collections::HashSet<String> = hierarchy
            .iter()
            .flat_map(|class| class.classes.keys().cloned())
            .collect();
        for member_name in member_names {
            let Some(effective_member_def_id) =
                find_member_type_in_class(tree, effective_package, &member_name)
                    .and_then(|member| member.def_id)
            else {
                continue;
            };
            for inherited_member_def_id in hierarchy.iter().filter_map(|class| {
                class
                    .classes
                    .get(&member_name)
                    .and_then(|member| member.def_id)
            }) {
                self.insert_alias(
                    ast::QualifiedName::from_ident(&member_name),
                    Some(inherited_member_def_id),
                    effective_member_def_id,
                );
            }
        }
    }
}
