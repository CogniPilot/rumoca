//! Type-table normalization helpers shared by the late typecheck pass: alias
//! root resolution, type-name rendering, and name/def-anchor lookup into the
//! type table.

use super::*;

impl TypeChecker {
    pub(crate) fn filter_non_value_component_type(
        type_table: &TypeTable,
        ty: TypeId,
    ) -> Option<TypeId> {
        match type_table.get(ty) {
            Some(Type::Class(class_ty)) if class_ty.kind == ClassKind::Package => None,
            _ => Some(ty),
        }
    }

    pub(crate) fn is_unresolved_alias_root(type_table: &TypeTable, ty: TypeId) -> bool {
        matches!(
            type_table.get(ty),
            Some(Type::Alias(alias)) if alias.aliased.is_unknown() || alias.aliased == ty
        )
    }

    pub(crate) fn resolve_alias_root(type_table: &TypeTable, mut ty: TypeId) -> TypeId {
        const MAX_DEPTH: usize = 16;
        for _ in 0..MAX_DEPTH {
            let Some(Type::Alias(alias)) = type_table.get(ty) else {
                return ty;
            };
            if alias.aliased.is_unknown() || alias.aliased == ty {
                return ty;
            }
            ty = alias.aliased;
        }
        ty
    }

    pub(crate) fn resolve_type_root(&self, type_table: &TypeTable, ty: TypeId) -> TypeId {
        self.type_roots
            .get(&ty)
            .copied()
            .unwrap_or_else(|| Self::resolve_alias_root(type_table, ty))
    }

    pub(crate) fn format_type_name(type_table: &TypeTable, type_id: TypeId) -> String {
        type_table
            .get(type_id)
            .and_then(|ty| ty.name().map(ToOwned::to_owned))
            .unwrap_or_else(|| format!("{type_id:?}"))
    }

    /// Resolve a type name to a TypeId.
    pub(crate) fn resolve_type_name(
        &self,
        name: &str,
        type_def_id: Option<DefId>,
        type_table: &TypeTable,
    ) -> TypeId {
        // Prefer DefId-based resolution for user-defined types.
        if let Some(type_id) = type_def_id
            .and_then(|def_id| self.resolve_type_from_def_anchor(def_id, name, type_table))
        {
            return type_id;
        }

        // Fall back to direct name lookup (builtins and fully-qualified names).
        if let Some(type_id) = type_table.lookup(name) {
            return type_id;
        }

        // Fall back to a unique dotted-suffix match.
        // This supports imported names like `SI.Reluctance` or `StateSelect`
        // when the type table stores canonical qualified names.
        if let Some(type_id) = self.type_suffix_index.get(name).copied().flatten() {
            return type_id;
        }

        // Last resort: unique short-name lookup.
        // Keep this as a compatibility fallback for mixed qualification styles.
        let short_name = crate::path_utils::class_name_leaf(name);
        if let Some(type_id) = self.type_suffix_index.get(short_name).copied().flatten() {
            return type_id;
        }

        TypeId::UNKNOWN
    }

    pub(crate) fn resolve_type_from_def_anchor(
        &self,
        def_id: DefId,
        name: &str,
        type_table: &TypeTable,
    ) -> Option<TypeId> {
        if crate::path_utils::is_qualified_class_name(name)
            && let Some(type_id) = self.resolve_dotted_type_from_anchor(def_id, name, type_table)
        {
            return Some(type_id);
        }
        // If we only have a first-segment anchor (e.g. `Medium`), keep
        // the anchor type instead of failing hard to UNKNOWN. Later checks
        // treat package/class anchors conservatively.
        self.type_ids_by_def_id.get(&def_id).copied()
    }

    pub(crate) fn resolve_dotted_type_from_anchor(
        &self,
        anchor_def_id: DefId,
        dotted_name: &str,
        type_table: &TypeTable,
    ) -> Option<TypeId> {
        let (_, tail) = crate::path_utils::class_root_split(dotted_name)?;
        let anchor_qname = self.def_qualified_names.get(&anchor_def_id)?;
        let candidate = format!("{anchor_qname}.{tail}");
        type_table.lookup(&candidate).or_else(|| {
            self.type_suffix_index
                .get(candidate.as_str())
                .copied()
                .flatten()
        })
    }
}
