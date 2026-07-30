//! Phase 2b: Contents Resolution - resolve equations, statements, expressions.
//!
//! This phase resolves component references in equations, algorithms, and
//! component start/modification expressions.

use crate::Resolver;
use crate::traversal_adapter::{
    ResolveTraversalCallbacks, walk_equations, walk_expression, walk_expressions, walk_statements,
    walk_subscripts,
};
use rumoca_core::{ComponentPath, DefId, ScopeId};
use rumoca_ir_ast as ast;

type ClassDef = ast::ClassDef;
type ComponentReference = ast::ComponentReference;
type Expression = ast::Expression;
type ScopeKind = ast::ScopeKind;
type StoredDefinition = ast::StoredDefinition;

impl ResolveTraversalCallbacks for Resolver {
    fn create_loop_scope(&mut self, enclosing: ScopeId) -> ScopeId {
        self.scope_tree.create_scope(enclosing, ScopeKind::ForLoop)
    }

    fn bind_loop_index_name(&mut self, loop_scope: ScopeId, index_name: &str) {
        let def_id = self.alloc_def_id(None, index_name);
        self.scope_tree.add_member(
            loop_scope,
            ComponentPath::from_flat_path(index_name),
            def_id,
        );
    }

    fn on_component_reference(&mut self, comp: &mut ComponentReference, scope: ScopeId) {
        self.resolve_component_reference(comp, scope);
    }

    fn on_function_reference(&mut self, comp: &mut ComponentReference, scope: ScopeId) {
        self.resolve_function_reference(comp, scope);
    }
}

impl Resolver {
    /// Resolve equations, statements, expressions in a StoredDefinition (Phase 2b).
    pub(crate) fn resolve_contents_all(
        &mut self,
        def: &mut StoredDefinition,
        scope: ScopeId,
        prefix: &str,
    ) {
        for (name, class) in def.classes.iter_mut() {
            let qualified_name = if prefix.is_empty() {
                name.clone()
            } else {
                format!("{}.{}", prefix, name)
            };
            self.resolve_contents_class(class, scope, &qualified_name);
        }
    }

    /// Resolve equations, statements, expressions in a ClassDef (Phase 2b).
    pub(crate) fn resolve_contents_class(
        &mut self,
        class: &mut ClassDef,
        enclosing_scope: ScopeId,
        qualified_name: &str,
    ) {
        let class_scope = class
            .scope_id
            .expect("Class scope should be set in registration phase");

        if let Some(constrainedby) = class.constrainedby.as_mut()
            && let Some(def_id) = self.resolve_qualified_name(constrainedby, class_scope)
        {
            constrainedby.def_id = Some(def_id);
        }

        let short_class_modifier_scope =
            (class.end_name_token.is_none() && !class.encapsulated).then_some(enclosing_scope);
        for ext in class.extends.iter_mut() {
            for modification in ext.modifications.iter_mut() {
                Self::resolve_extend_modification(
                    self,
                    &mut modification.expr,
                    class_scope,
                    short_class_modifier_scope.unwrap_or(class_scope),
                );
            }
        }

        self.resolve_subscripts(&mut class.array_subscripts, class_scope);

        // Resolve component references in equations and algorithms
        // MLS §5.3: Full name lookup happens during instantiation/flattening,
        // but we can do partial resolution here for the Class Tree.
        walk_equations(self, &mut class.equations, class_scope);
        walk_equations(self, &mut class.initial_equations, class_scope);
        for algorithm_section in class.algorithms.iter_mut() {
            walk_statements(self, algorithm_section, class_scope);
        }
        for algorithm_section in class.initial_algorithms.iter_mut() {
            walk_statements(self, algorithm_section, class_scope);
        }

        if let Some(external) = class.external.as_mut() {
            if let Some(output) = external.output.as_mut() {
                self.resolve_component_reference(output, class_scope);
            }
            self.resolve_expressions(&mut external.args, class_scope);
        }

        // Resolve component start/modification expressions and type names
        for (_name, comp) in class.components.iter_mut() {
            self.resolve_expression(&mut comp.start, class_scope);
            if let Some(binding) = &mut comp.binding {
                self.resolve_expression(binding, class_scope);
            }
            for mod_expr in comp.modifications.values_mut() {
                self.resolve_expression(mod_expr, class_scope);
            }
            self.resolve_subscripts(&mut comp.shape_expr, class_scope);
            if let Some(ref mut cond) = comp.condition {
                self.resolve_expression(cond, class_scope);
            }
            if let Some(constrainedby) = comp.constrainedby.as_mut()
                && let Some(def_id) = self.resolve_qualified_name(constrainedby, class_scope)
            {
                constrainedby.def_id = Some(def_id);
            }

            // Resolve the component's type name to its DefId (MLS §5.3).
            // This enables O(1) type lookup during instantiation instead of string-based search.
            // Builtins are registered in global scope, so normal lookup finds them.
            if let Some(type_def_id) = self.resolve_qualified_name(&comp.type_name, class_scope) {
                // Full resolution succeeded
                comp.type_name.def_id = Some(type_def_id);
                comp.type_def_id = Some(type_def_id);
                self.stats.types_fully_resolved += 1;
            } else if let Some(type_def_id) =
                self.resolve_type_name_with_inheritance(&comp.type_name, class_scope)
            {
                // Full resolution via inherited members succeeded.
                comp.type_name.def_id = Some(type_def_id);
                comp.type_def_id = Some(type_def_id);
                self.stats.types_fully_resolved += 1;
            } else if !comp.type_name.name.is_empty() {
                // Full resolution failed. Try partial resolution (MLS §7.3).
                self.try_partial_type_resolution(comp, class_scope, qualified_name);
            }
        }

        // Recursively resolve nested classes' contents
        for (name, nested) in class.classes.iter_mut() {
            let nested_qualified = format!("{}.{}", qualified_name, name);
            self.resolve_contents_class(nested, class_scope, &nested_qualified);
        }
    }

    /// Resolve one modification of an `extends` clause.
    ///
    /// Resolve an extends modification without conflating its target and value
    /// environments.
    ///
    /// MLS §4.6.1: a non-encapsulated short class does not introduce an
    /// additional lexical scope for its modifiers. Consequently the modifier
    /// target is resolved against the derived/base class while every value is
    /// resolved in the enclosing instance scope. Long-form and encapsulated
    /// short-class modifiers use the class scope for both.
    fn resolve_extend_modification(
        &mut self,
        expr: &mut Expression,
        target_scope: ScopeId,
        value_scope: ScopeId,
    ) {
        match expr {
            Expression::NamedArgument { value, .. } => {
                self.resolve_expression(std::sync::Arc::make_mut(value), value_scope);
            }
            Expression::Modification { target, value, .. } => {
                self.resolve_component_reference(target, target_scope);
                self.resolve_expression(std::sync::Arc::make_mut(value), value_scope);
            }
            other => self.resolve_expression(other, target_scope),
        }
    }

    /// Try partial type resolution for qualified names (MLS §7.3).
    ///
    /// For types like `Medium.AbsolutePressure` where `Medium` is a replaceable
    /// package, we can't fully resolve until instantiation when the actual
    /// redeclared type is known. Set type_name.def_id to the first part's DefId
    /// to indicate partial resolution succeeded.
    fn try_partial_type_resolution(
        &mut self,
        comp: &mut rumoca_ir_ast::Component,
        class_scope: ScopeId,
        qualified_name: &str,
    ) {
        let first_part = &comp.type_name.name[0].text;

        // First check direct scope lookup
        if let Some(first_def_id) = self
            .scope_tree
            .lookup(class_scope, &ComponentPath::from_flat_path(first_part))
            && self.partial_type_root_ids.contains(&first_def_id)
        {
            comp.type_name.def_id = Some(first_def_id);
            self.stats.types_partial_direct += 1;
            return;
        }

        // If not in direct scope, check inherited members from base classes.
        // We need to search the entire enclosing class hierarchy.
        if let Some(def_id) = self.find_inherited_type(class_scope, first_part)
            && self.partial_type_root_ids.contains(&def_id)
        {
            comp.type_name.def_id = Some(def_id);
            self.stats.types_partial_inherited += 1;
            return;
        }

        // Failed to resolve
        self.stats.types_unresolved += 1;
        self.stats
            .types_unresolved_details
            .push((comp.type_name.to_string(), qualified_name.to_string()));
    }

    /// DefIds of the classes enclosing `scope`, innermost first, walked through
    /// the scope tree (MLS §5.3: name lookup proceeds through enclosing scopes,
    /// which are structure — never re-derived from rendered names).
    pub(crate) fn enclosing_class_def_ids(&self, scope: ScopeId) -> impl Iterator<Item = DefId> {
        std::iter::successors(Some(scope), |current| self.scope_tree.parent(*current))
            .filter_map(|current| self.scope_to_class_def.get(&current).copied())
    }

    /// The class that lexically encloses the class declared as `class_def_id`.
    ///
    /// MLS §5.3.2 looks a simple name up in the enclosing class after the class
    /// itself; that enclosing class is the parent scope's owner in the scope
    /// tree (SPEC_0002), which is the container's own `DefId` (SPEC_0001), not
    /// the leading segments of a rendered qualified name.
    pub(crate) fn enclosing_class_def_id(&self, class_def_id: DefId) -> Option<DefId> {
        let class_scope = *self.class_def_scopes.get(&class_def_id)?;
        let enclosing_scope = self.scope_tree.parent(class_scope)?;
        self.enclosing_class_def_ids(enclosing_scope).next()
    }

    /// Find an inherited type by searching the class at `scope` and every
    /// enclosing class.
    fn find_inherited_type(&self, scope: ScopeId, type_name: &str) -> Option<rumoca_core::DefId> {
        self.enclosing_class_def_ids(scope)
            .find_map(|container| self.lookup_inherited_member_of(container, type_name))
    }

    /// Resolve references in a list of expressions.
    fn resolve_expressions(&mut self, exprs: &mut [Expression], scope: ScopeId) {
        walk_expressions(self, exprs, scope);
    }

    /// Resolve references in an expression.
    pub(crate) fn resolve_expression(&mut self, expr: &mut Expression, scope: ScopeId) {
        walk_expression(self, expr, scope);
    }

    /// Resolve a component reference.
    ///
    /// MLS §5.3.1: Simple name lookup starts in the current scope and
    /// proceeds to enclosing scopes. For composite names (a.b.c), we
    /// only resolve the first part here; full resolution happens during
    /// instantiation when the instance tree is available.
    pub(crate) fn resolve_component_reference(
        &mut self,
        comp: &mut ComponentReference,
        scope: ScopeId,
    ) {
        if comp.parts.is_empty() {
            return;
        }

        // Get the first part of the reference
        let first_name = &comp.parts[0].ident.text;

        // Look up the name in the scope tree
        if let Some(def_id) = self
            .scope_tree
            .lookup(scope, &ComponentPath::from_flat_path(first_name))
        {
            comp.def_id = Some(def_id);
            self.stats.comp_refs_resolved += 1;
        } else {
            self.stats.comp_refs_unresolved += 1;
        }
        comp.target_def_id = self.resolve_component_reference_full_path(comp, scope);
        // Note: We don't report undefined references here because:
        // 1. The name might be from an import that hasn't been resolved yet
        // 2. The name might be from a base class (extends)
        // 3. Full MLS name lookup happens during instantiation/flattening
        // Errors will be reported during type checking or instantiation.

        // Also resolve subscript expressions
        for part in comp.parts.iter_mut() {
            if let Some(subs) = &mut part.subs {
                self.resolve_subscripts(subs, scope);
            }
        }
    }

    /// Resolve a function reference to its callable DefId while preserving the
    /// source component-reference parts for later scope-sensitive phases.
    ///
    /// Unlike generic component references, function calls should resolve the
    /// entire path (including inherited package members) at resolve time so
    /// later phases do exact function lookup without name heuristics.
    fn resolve_function_reference(&mut self, comp: &mut ComponentReference, scope: ScopeId) {
        self.resolve_component_reference(comp, scope);

        let Some(resolved_def_id) = comp.target_def_id else {
            // A leading declaration is not proof that the called member exists.
            // Replaceable-package tails remain unresolved until instantiation
            // proves the concrete member.
            comp.target_def_id = None;
            return;
        };

        comp.target_def_id = Some(resolved_def_id);
    }

    fn resolve_function_first_part(&self, first_part: &str, scope: ScopeId) -> Option<DefId> {
        if let Some(def_id) = self
            .scope_tree
            .lookup(scope, &ComponentPath::from_flat_path(first_part))
        {
            return Some(def_id);
        }

        self.enclosing_class_def_ids(scope)
            .find_map(|container| self.lookup_inherited_member_of(container, first_part))
    }

    fn resolve_component_reference_full_path(
        &self,
        comp: &ComponentReference,
        scope: ScopeId,
    ) -> Option<rumoca_core::DefId> {
        let first_part = comp.parts.first()?.ident.text.as_ref();
        let mut current_def_id = comp
            .def_id
            .or_else(|| self.resolve_function_first_part(first_part, scope))?;

        for part in comp.parts.iter().skip(1) {
            let container_scope = *self.class_def_scopes.get(&current_def_id)?;
            current_def_id = self.scope_tree.lookup_member(
                container_scope,
                &ComponentPath::from_flat_path(&part.ident.text),
            )?;
        }

        Some(current_def_id)
    }

    fn resolve_type_name_with_inheritance(
        &self,
        name: &rumoca_ir_ast::Name,
        scope: ScopeId,
    ) -> Option<rumoca_core::DefId> {
        let first_part = name.name.first()?.text.as_ref();
        let mut current_def_id = self
            .scope_tree
            .lookup(scope, &ComponentPath::from_flat_path(first_part))
            .or_else(|| self.resolve_function_first_part(first_part, scope))
            // MLS §7.3: inherited class/type elements are visible as members of
            // the extending class, including simple type names in nested records.
            .or_else(|| self.find_inherited_type(scope, first_part))?;
        let mut current_qualified = self.def_names.get(&current_def_id)?.clone();

        for part in name.name.iter().skip(1) {
            let member = part.text.as_ref();
            let direct_name = format!("{current_qualified}.{member}");
            if let Some(&next_def_id) = self.name_to_def.get(&direct_name) {
                current_def_id = next_def_id;
                current_qualified = self.def_names.get(&next_def_id)?.clone();
                continue;
            }

            let inherited_def_id = self.lookup_inherited_member_of(current_def_id, member)?;
            current_def_id = inherited_def_id;
            current_qualified = self.def_names.get(&inherited_def_id)?.clone();
        }

        Some(current_def_id)
    }

    /// Resolve references in a list of subscripts.
    fn resolve_subscripts(&mut self, subs: &mut [rumoca_ir_ast::Subscript], scope: ScopeId) {
        walk_subscripts(self, subs, scope);
    }
}
