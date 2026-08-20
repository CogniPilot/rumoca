//! Name lookup helpers for name resolution.
//!
//! This module provides functions for resolving qualified names and
//! looking up inherited members during extends resolution.

use crate::Resolver;
use indexmap::IndexSet;
use rumoca_core::{ComponentPath, DefId, ScopeId};
use std::collections::HashSet;

impl Resolver {
    /// Resolve a qualified name (e.g., "Package.Model" or "Model").
    ///
    /// For simple names, uses scope lookup.
    /// For qualified names, resolves the first part via scope lookup,
    /// then uses O(1) map lookup for subsequent parts.
    pub(crate) fn resolve_qualified_name(
        &self,
        name: &rumoca_ir_ast::Name,
        scope: ScopeId,
    ) -> Option<DefId> {
        self.resolve_qualified_name_excluding(name, scope, None)
    }

    /// Resolve a qualified name, optionally excluding a specific DefId from results.
    ///
    /// This is used for extends resolution where we don't want a class to find itself.
    /// For example, `record ThermodynamicState extends ThermodynamicState` should find
    /// the parent package's ThermodynamicState, not the class being defined.
    ///
    /// The exclusion only applies to simple (single-part) names. For qualified names like
    /// `ModelicaReference.Icons.Information`, we need to navigate into the first part
    /// even if it matches the excluded DefId, so exclusion is not applied.
    ///
    /// Pass `None` for `exclude` to perform a normal lookup without exclusion.
    pub(crate) fn resolve_qualified_name_excluding(
        &self,
        name: &rumoca_ir_ast::Name,
        scope: ScopeId,
        exclude: Option<DefId>,
    ) -> Option<DefId> {
        if name.name.is_empty() {
            return None;
        }

        // Get the first part of the name
        let first_part = &name.name[0].text;
        let first_path = ComponentPath::from_flat_path(first_part);

        // Only apply exclusion for simple (single-part) names.
        // For qualified names like `Pkg.Class`, we need to navigate into `Pkg` even if
        // it matches the excluded DefId (e.g., `package Pkg extends Pkg.Icons.Info`).
        let effective_exclude = if name.name.len() == 1 { exclude } else { None };

        // Look up the first part in the scope chain
        let mut current_def_id =
            self.scope_tree
                .lookup_excluding(scope, &first_path, effective_exclude)?;

        // If there are more parts, navigate nested classes using O(1) lookup
        for part in name.name.iter().skip(1) {
            // Get the current qualified name
            let current_qualified = self.def_names.get(&current_def_id)?;

            // Build the next qualified name
            let next_qualified = format!("{}.{}", current_qualified, part.text);

            // O(1) lookup using the inverse map
            if let Some(next_def_id) = self.name_to_def.get(&next_qualified) {
                current_def_id = *next_def_id;
                continue;
            }

            // Fallback: the current node may be a short class definition alias
            // (e.g. `package PS = PhaseSystem;`) where members are inherited from
            // the aliased base class. Those inherited members are not explicitly
            // declared under `current_qualified.*` in `name_to_def`.
            //
            // MLS §7.3 semantics require dotted access like `PS.j` to resolve.
            // Reuse inherited-member lookup for this dotted navigation step.
            if self.class_types.contains_key(&current_def_id)
                && let Some(inherited_def_id) =
                    self.lookup_inherited_member_of(current_def_id, &part.text)
            {
                current_def_id = inherited_def_id;
                continue;
            }

            return None;
        }

        Some(current_def_id)
    }

    /// Look up a member name in a class's inheritance chain (recursive).
    ///
    /// MLS §7.3: Used for the "redeclare extends SameName" pattern where a nested
    /// class extends an INHERITED class with the same short name. Since inherited
    /// members aren't in our scope tree, we search the containing class's base
    /// classes for the member.
    ///
    /// Example:
    /// ```modelica
    /// package Base
    ///     record State end State;
    /// end Base;
    /// package Derived extends Base
    ///     redeclare record extends State end State;  // State from Base
    /// end Derived;
    /// ```
    ///
    /// When resolving `State` in `Derived.State extends State`, normal lookup fails
    /// because `State` is inherited (not directly declared). This function searches
    /// `Derived`'s base classes (`Base`) for a member named `State`.
    ///
    /// For deep inheritance chains (e.g., WaterIF97_pT → WaterIF97_base →
    /// PartialTwoPhaseMedium → PartialPureSubstance → PartialMedium), the search
    /// is recursive to find members from any ancestor class.
    pub(crate) fn lookup_inherited_member_of(
        &self,
        container: DefId,
        member_name: &str,
    ) -> Option<DefId> {
        let mut candidates = IndexSet::new();
        self.collect_inherited_member_candidates(
            container,
            member_name,
            &mut HashSet::new(),
            &mut candidates,
        );
        (candidates.len() == 1)
            .then(|| candidates.first().copied())
            .flatten()
    }

    /// Name-keyed adapter over [`Resolver::lookup_inherited_member_of`].
    ///
    /// Only for call sites that hold nothing but a rendered scope path (the
    /// unresolved-reference report, whose records carry scope text rather than
    /// resolved ids). Semantic call sites must pass the container's `DefId`.
    #[cfg(test)]
    pub(crate) fn lookup_inherited_member(
        &self,
        container_qualified_name: &str,
        member_name: &str,
    ) -> Option<DefId> {
        let container = *self.name_to_def.get(container_qualified_name)?;
        self.lookup_inherited_member_of(container, member_name)
    }

    /// Collect every visible inherited declaration with cycle detection.
    ///
    /// A direct declaration in one base hides that base's ancestors, while
    /// declarations from distinct bases remain separate candidates. Returning
    /// only a unique candidate prevents multiple inheritance from selecting an
    /// arbitrary redeclare target.
    fn collect_inherited_member_candidates(
        &self,
        container: DefId,
        member_name: &str,
        visited: &mut HashSet<DefId>,
        candidates: &mut IndexSet<DefId>,
    ) {
        if !visited.insert(container) {
            return;
        }

        let Some(base_ids) = self.class_to_bases.get(&container) else {
            return;
        };
        for base_id in base_ids {
            self.collect_base_member_candidate(*base_id, member_name, visited, candidates);
        }
    }

    fn collect_base_member_candidate(
        &self,
        base_id: DefId,
        member_name: &str,
        visited: &mut HashSet<DefId>,
        candidates: &mut IndexSet<DefId>,
    ) {
        let Some(base_qualified) = self.def_names.get(&base_id) else {
            return;
        };
        // Composing `Base.Member` addresses the base's own declaration table;
        // the base itself is already identified by `base_id`.
        let inherited_name = format!("{}.{}", base_qualified, member_name);
        if let Some(&inherited_def_id) = self.name_to_def.get(&inherited_name) {
            candidates.insert(inherited_def_id);
            return;
        }
        self.collect_inherited_member_candidates(base_id, member_name, visited, candidates);
    }
}
