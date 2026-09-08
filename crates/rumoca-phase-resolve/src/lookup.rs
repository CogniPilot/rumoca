//! Name lookup helpers for name resolution.
//!
//! This module provides functions for resolving qualified names and
//! looking up inherited members during extends resolution.

use crate::Resolver;
use rumoca_core::{ComponentPath, DefId, Diagnostic, PrimaryLabel, ScopeId, Span};
use rumoca_ir_ast::LookupOutcome;

impl Resolver {
    pub(crate) fn emit_ambiguous_inherited_lookup(&mut self, name: &str, span: Span) {
        self.diagnostics.emit(Diagnostic::error(
            "ER002",
            format!("ambiguous inherited reference: '{name}'"),
            PrimaryLabel::new(span).with_message("multiple inherited declarations match this name"),
        ));
    }

    pub(crate) fn emit_ambiguous_unqualified_import(&mut self, name: &str, span: Span) {
        self.diagnostics.emit(Diagnostic::error(
            "ER112",
            format!(
                "'{name}' is provided by more than one unqualified import and is ambiguous (MLS §5.3.1)"
            ),
            PrimaryLabel::new(span).with_message("qualify the name or use a selective import"),
        ));
    }

    /// Resolve a qualified name (e.g., "Package.Model" or "Model").
    ///
    /// For simple names, uses scope lookup.
    /// For qualified names, resolves the first part via lexical scope lookup,
    /// then traverses exact class scopes for subsequent parts.
    pub(crate) fn resolve_qualified_name(
        &self,
        name: &rumoca_ir_ast::Name,
        scope: ScopeId,
    ) -> LookupOutcome {
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
    ) -> LookupOutcome {
        if name.name.is_empty() {
            return LookupOutcome::Absent;
        }

        let first_part = &name.name[0].text;
        let first_path = ComponentPath::from_flat_path(first_part);

        // Only apply exclusion for simple (single-part) names.
        // For qualified names like `Pkg.Class`, we need to navigate into `Pkg` even if
        // it matches the excluded DefId (e.g., `package Pkg extends Pkg.Icons.Info`).
        let effective_exclude = if name.name.len() == 1 { exclude } else { None };

        // Look up the first part in the scope chain
        let mut current_def_id =
            match self
                .scope_tree
                .lookup_excluding(scope, &first_path, effective_exclude)
            {
                LookupOutcome::Found(definition) => definition,
                outcome => return outcome,
            };

        // Once the head is a declaration, every tail segment is a member lookup
        // in that declaration's exact class scope. The scope owns both direct
        // and effective inherited members, including ambiguity.
        for part in name.name.iter().skip(1) {
            current_def_id = match self.lookup_class_member(current_def_id, &part.text) {
                LookupOutcome::Found(definition) => definition,
                outcome => return outcome,
            };
        }

        LookupOutcome::Found(current_def_id)
    }

    /// Look up one member in a declaration's authoritative class scope.
    pub(crate) fn lookup_class_member(&self, container: DefId, member_name: &str) -> LookupOutcome {
        let Some(scope) = self.class_def_scopes.get(&container).copied() else {
            return LookupOutcome::Absent;
        };
        self.scope_tree
            .lookup_member(scope, &ComponentPath::from_parts([member_name]))
    }

    /// Look up only the effective inherited slot in one class scope.
    ///
    /// Redeclaration must see through the redeclaring direct member to the
    /// inherited declaration it replaces. The scope tree already owns the
    /// reconciled unique/ambiguous state, so this query does not walk bases.
    pub(crate) fn lookup_inherited_class_member(
        &self,
        container: DefId,
        member_name: &str,
    ) -> LookupOutcome {
        let Some(scope) = self.class_def_scopes.get(&container).copied() else {
            return LookupOutcome::Absent;
        };
        match self
            .scope_tree
            .inherited_member(scope, &ComponentPath::from_parts([member_name]))
        {
            Some(rumoca_ir_ast::InheritedMember::Unique(def_id)) => LookupOutcome::Found(def_id),
            Some(rumoca_ir_ast::InheritedMember::Ambiguous) => LookupOutcome::AmbiguousInherited,
            None => LookupOutcome::Absent,
        }
    }
}
