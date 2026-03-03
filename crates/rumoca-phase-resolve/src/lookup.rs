//! Name lookup helpers for name resolution.
//!
//! This module provides functions for resolving qualified names and
//! looking up inherited members during extends resolution.

use crate::Resolver;
use rumoca_core::{DefId, ScopeId};
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

        // Only apply exclusion for simple (single-part) names.
        // For qualified names like `Pkg.Class`, we need to navigate into `Pkg` even if
        // it matches the excluded DefId (e.g., `package Pkg extends Pkg.Icons.Info`).
        let effective_exclude = if name.name.len() == 1 { exclude } else { None };

        // Look up the first part in the scope chain
        let mut current_def_id =
            self.scope_tree
                .lookup_excluding(scope, first_part, effective_exclude)?;

        // If there are more parts, navigate nested classes using O(1) lookup
        for part in name.name.iter().skip(1) {
            // Get the current qualified name
            let current_qualified = self.def_names.get(&current_def_id)?;

            // Build the next qualified name
            let next_qualified = format!("{}.{}", current_qualified, part.text);

            // O(1) lookup using the inverse map
            current_def_id = *self.name_to_def.get(&next_qualified)?;
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
    pub(crate) fn lookup_inherited_member(
        &self,
        container_qualified_name: &str,
        member_name: &str,
    ) -> Option<DefId> {
        self.lookup_inherited_member_recursive(
            container_qualified_name,
            member_name,
            &mut HashSet::new(),
        )
    }

    /// Recursive helper for inherited member lookup with cycle detection.
    ///
    /// Uses the `class_to_bases` index for O(1) base class lookup instead of
    /// iterating through all inheritance edges.
    fn lookup_inherited_member_recursive(
        &self,
        container_qualified_name: &str,
        member_name: &str,
        visited: &mut HashSet<String>,
    ) -> Option<DefId> {
        // Avoid infinite loops in case of circular inheritance
        if !visited.insert(container_qualified_name.to_string()) {
            return None;
        }

        // Get container class's DefId
        let container_def_id = self.name_to_def.get(container_qualified_name)?;

        // O(1) lookup of base classes using the index
        let base_ids = self.class_to_bases.get(container_def_id)?;

        for base_id in base_ids {
            if let Some(result) = self.check_base_for_member(base_id, member_name, visited) {
                return Some(result);
            }
        }

        None
    }

    /// Check a single base class for an inherited member.
    fn check_base_for_member(
        &self,
        base_id: &DefId,
        member_name: &str,
        visited: &mut HashSet<String>,
    ) -> Option<DefId> {
        let base_qualified = self.def_names.get(base_id)?;

        // Check if base_qualified.member_name exists directly
        let inherited_name = format!("{}.{}", base_qualified, member_name);
        if let Some(&inherited_def_id) = self.name_to_def.get(&inherited_name) {
            return Some(inherited_def_id);
        }

        // Recursively search the base class's inheritance chain
        self.lookup_inherited_member_recursive(base_qualified, member_name, visited)
    }
}
