//! Effective virtual-class selections for one instantiation scope.
//!
//! [`TypeOverrideMap`] records which concrete class each replaceable class or
//! package alias currently selects (MLS §7.3). Selections are keyed only by the
//! alias declaration identity proved by Resolve. Source spelling is not a safe
//! key: two lexical scopes may contain same-spelled aliases.

use crate::type_lookup::find_member_type_in_class;
use crate::{InstantiateError, InstantiateResult};
use rumoca_core::{DefId, Span};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct TypeOverrideMap {
    targets_by_alias_def_id: IndexMap<DefId, DefId>,
}

fn is_predefined_identity(tree: &ast::ClassTree, def_id: DefId) -> bool {
    rumoca_core::BUILTIN_TYPES.iter().any(|name| {
        tree.scope_tree
            .predefined_member(&rumoca_core::ComponentPath::from_flat_path(name))
            == Some(def_id)
    })
}

fn plan_inherited_member_aliases(
    tree: &ast::ClassTree,
    effective_package: &ast::ClassDef,
    hierarchy: &[&ast::ClassDef],
    member_name: &str,
    planned_aliases: &mut Vec<(DefId, DefId)>,
) -> InstantiateResult<()> {
    let Some(effective_member) = find_member_type_in_class(tree, effective_package, member_name)?
    else {
        return Err(Box::new(InstantiateError::missing_resolved_identity(
            format!(
                "effective nested member `{member_name}` of `{}`",
                effective_package.name.text
            ),
            effective_package.location.span(),
        )));
    };
    let effective_member_def_id = effective_member.def_id.ok_or_else(|| {
        Box::new(InstantiateError::missing_resolved_identity(
            format!("effective nested member `{member_name}`"),
            effective_member.location.span(),
        ))
    })?;
    for class in hierarchy {
        let Some(inherited_member) = class.classes.get(member_name) else {
            continue;
        };
        let inherited_member_def_id = inherited_member.def_id.ok_or_else(|| {
            Box::new(InstantiateError::missing_resolved_identity(
                format!(
                    "nested member `{member_name}` inherited from `{}`",
                    class.name.text
                ),
                inherited_member.location.span(),
            ))
        })?;
        planned_aliases.push((inherited_member_def_id, effective_member_def_id));
    }
    Ok(())
}

impl TypeOverrideMap {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    #[cfg(test)]
    pub(crate) fn is_empty(&self) -> bool {
        self.targets_by_alias_def_id.is_empty()
    }

    pub(crate) fn insert_alias(&mut self, alias_def_id: DefId, target_def_id: DefId) {
        self.targets_by_alias_def_id
            .insert(alias_def_id, target_def_id);
    }

    pub(super) fn insert_alias_if_absent(&mut self, alias_def_id: DefId, target_def_id: DefId) {
        self.targets_by_alias_def_id
            .entry(alias_def_id)
            .or_insert(target_def_id);
    }

    pub(crate) fn insert_class_override(&mut self, class_override: &ast::ClassOverride) {
        self.insert_alias(class_override.alias_def_id, class_override.target_def_id);
    }

    pub(crate) fn extend_from(&mut self, other: &TypeOverrideMap) {
        for (alias_def_id, target_def_id) in &other.targets_by_alias_def_id {
            self.targets_by_alias_def_id
                .insert(*alias_def_id, *target_def_id);
        }
    }

    pub(crate) fn target_for_alias_def_id(&self, alias_def_id: DefId) -> Option<DefId> {
        self.targets_by_alias_def_id.get(&alias_def_id).copied()
    }

    pub(crate) fn target_for_reference(
        &self,
        tree: &ast::ClassTree,
        reference: &ast::ComponentReference,
    ) -> InstantiateResult<Option<DefId>> {
        let alias_def_id = reference.root_def_id().ok_or_else(|| {
            Box::new(InstantiateError::missing_resolved_identity(
                format!("virtual-class reference `{reference}`"),
                reference.span,
            ))
        })?;
        self.checked_target_for_alias_def_id(tree, alias_def_id, reference.span)
    }

    pub(crate) fn checked_target_for_alias_def_id(
        &self,
        tree: &ast::ClassTree,
        alias_def_id: DefId,
        span: Span,
    ) -> InstantiateResult<Option<DefId>> {
        let alias_class = tree.get_class_by_def_id(alias_def_id);
        if alias_class.is_none() && !is_predefined_identity(tree, alias_def_id) {
            return Err(Box::new(InstantiateError::missing_resolved_identity(
                format!("virtual-class alias {alias_def_id:?}"),
                span,
            )));
        }
        let Some(target_def_id) = self.target_for_alias_def_id(alias_def_id) else {
            return Ok(None);
        };
        let target_class = tree.get_class_by_def_id(target_def_id);
        if target_class.is_none() && !is_predefined_identity(tree, target_def_id) {
            return Err(Box::new(InstantiateError::missing_resolved_identity(
                format!("virtual-class target {target_def_id:?}"),
                span,
            )));
        }
        Ok(Some(target_def_id))
    }

    /// Materialize the effective virtual-class selections for instance IR.
    ///
    /// Identity mappings describe locally declared classes, not overrides, so
    /// only changed declaration slots are retained.
    pub(crate) fn class_overrides(
        &self,
        tree: &ast::ClassTree,
        publication_span: Span,
    ) -> InstantiateResult<ast::ClassOverrideMap> {
        let mut materialized = ast::ClassOverrideMap::default();
        for (alias_def_id, target_def_id) in &self.targets_by_alias_def_id {
            let alias_class = tree.get_class_by_def_id(*alias_def_id).ok_or_else(|| {
                Box::new(InstantiateError::missing_resolved_identity(
                    format!("virtual-class alias {alias_def_id:?}"),
                    publication_span,
                ))
            })?;
            let target_class = tree.get_class_by_def_id(*target_def_id);
            if target_class.is_none() && !is_predefined_identity(tree, *target_def_id) {
                return Err(Box::new(InstantiateError::missing_resolved_identity(
                    format!("virtual-class target {target_def_id:?}"),
                    publication_span,
                )));
            }
            if alias_def_id == target_def_id {
                continue;
            }
            materialized.insert(
                *alias_def_id,
                ast::ClassOverride::new(
                    alias_class.name.text.to_string(),
                    *alias_def_id,
                    *target_def_id,
                    None,
                ),
            );
        }
        Ok(materialized)
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
    ) -> InstantiateResult<()> {
        let effective_package = tree
            .get_class_by_def_id(effective_package_def_id)
            .ok_or_else(|| {
                Box::new(InstantiateError::ModelNotFound(format!(
                    "effective package {effective_package_def_id:?}"
                )))
            })?;
        let mut hierarchy = Vec::new();
        let mut to_visit = vec![effective_package];
        let mut visited = std::collections::HashSet::new();
        while let Some(class) = to_visit.pop() {
            let class_def_id = class.def_id.ok_or_else(|| {
                Box::new(InstantiateError::missing_resolved_identity(
                    format!("effective package hierarchy `{}`", class.name.text),
                    class.location.span(),
                ))
            })?;
            if !visited.insert(class_def_id) {
                continue;
            }
            to_visit.extend(super::class_hierarchy::extends_base_classes(tree, class)?);
            hierarchy.push(class);
        }

        let member_names: indexmap::IndexSet<String> = hierarchy
            .iter()
            .flat_map(|class| class.classes.keys().cloned())
            .collect();
        let mut planned_aliases = Vec::new();
        for member_name in member_names {
            plan_inherited_member_aliases(
                tree,
                effective_package,
                &hierarchy,
                &member_name,
                &mut planned_aliases,
            )?;
        }
        for (inherited_member_def_id, effective_member_def_id) in planned_aliases {
            self.insert_alias(inherited_member_def_id, effective_member_def_id);
        }
        Ok(())
    }
}
