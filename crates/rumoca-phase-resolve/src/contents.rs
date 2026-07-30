//! Phase 2b: Contents Resolution - resolve equations, statements, expressions.
//!
//! This phase resolves component references in equations, algorithms, and
//! component start/modification expressions.

use crate::Resolver;
use crate::traversal_adapter::{
    ResolveTraversalCallbacks, walk_equations, walk_expression, walk_expressions, walk_statements,
    walk_subscripts,
};
use rumoca_core::{ComponentPath, DefId, Diagnostic, PrimaryLabel, ScopeId};
use rumoca_ir_ast as ast;

type ClassDef = ast::ClassDef;
type ComponentReference = ast::ComponentReference;
type Expression = ast::Expression;
type ScopeKind = ast::ScopeKind;
type StoredDefinition = ast::StoredDefinition;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FullPathResolution {
    Exact(DefId),
    DeferredDynamic,
    MissingStaticTail,
    UnresolvedRoot,
}

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

    fn on_field_access(
        &mut self,
        base: &Expression,
        field: &str,
        field_def_id: &mut Option<DefId>,
        _scope: ScopeId,
    ) {
        *field_def_id = self.resolve_field_access_member(base, field);
    }
}

impl Resolver {
    /// Resolve component declaration types for the entire tree before walking
    /// any expressions.
    ///
    /// A qualified reference can cross components declared in a class that
    /// appears later in source order. This prepass makes those exact type
    /// identities available deterministically.
    pub(crate) fn resolve_component_types_all(&mut self, def: &mut StoredDefinition, prefix: &str) {
        for (name, class) in def.classes.iter_mut() {
            let qualified_name = if prefix.is_empty() {
                name.clone()
            } else {
                format!("{prefix}.{name}")
            };
            self.resolve_component_types_class(class, &qualified_name);
        }
    }

    fn resolve_component_types_class(&mut self, class: &mut ClassDef, qualified_name: &str) {
        let class_scope = class
            .scope_id
            .expect("class scope must be assigned before component type resolution");
        for component in class.components.values_mut() {
            self.resolve_component_type_identity(component, class_scope, qualified_name);
        }
        for (name, nested) in class.classes.iter_mut() {
            self.resolve_component_types_class(nested, &format!("{qualified_name}.{name}"));
        }
    }

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
            for source_modification in comp.source_modifications.iter_mut() {
                self.resolve_source_modification(source_modification, class_scope);
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
        }

        // Recursively resolve nested classes' contents
        for (name, nested) in class.classes.iter_mut() {
            let nested_qualified = format!("{}.{}", qualified_name, name);
            self.resolve_contents_class(nested, class_scope, &nested_qualified);
        }
    }

    fn resolve_component_type_identity(
        &mut self,
        comp: &mut ast::Component,
        class_scope: ScopeId,
        qualified_name: &str,
    ) {
        if comp.type_name.name.len() > 1
            && let Some(root_def_id) = self.dynamic_type_root(&comp.type_name, class_scope)
        {
            comp.type_name.def_id = Some(root_def_id);
            comp.type_def_id = None;
            // The declared class is only known once instantiation applies the
            // redeclare, so this declaration's own member set is
            // instance-dependent too. Recording it keeps a qualified tail such
            // as `medium.p` classified as deferred instead of decaying into a
            // missing static tail at Resolve.
            if let Some(component) = comp.def_id {
                self.dynamic_member_root_ids.insert(component);
            }
            self.stats.types_partial_direct += 1;
            return;
        }
        let resolved = self
            .resolve_qualified_name(&comp.type_name, class_scope)
            .or_else(|| self.resolve_type_name_with_inheritance(&comp.type_name, class_scope));
        if let Some(type_def_id) = resolved {
            comp.type_name.def_id = Some(type_def_id);
            comp.type_def_id = Some(type_def_id);
            self.stats.types_fully_resolved += 1;
        } else if !comp.type_name.name.is_empty() {
            self.try_partial_type_resolution(comp, class_scope, qualified_name);
        }
        if let (Some(component), Some(component_type)) = (comp.def_id, comp.type_def_id) {
            if self.dynamic_member_root_ids.contains(&component_type) {
                // Even a non-replaceable component has an instance-dependent
                // member set when its declared class is replaceable.
                self.dynamic_member_root_ids.insert(component);
            } else if !self.dynamic_member_root_ids.contains(&component) {
                self.component_type_def_ids
                    .insert(component, component_type);
            }
        }
    }

    fn dynamic_type_root(&self, name: &ast::Name, scope: ScopeId) -> Option<DefId> {
        let first = name.name.first()?.text.as_ref();
        self.scope_tree
            .lookup(scope, &ComponentPath::from_flat_path(first))
            .or_else(|| self.find_inherited_type(scope, first))
            .filter(|def_id| self.dynamic_member_root_ids.contains(def_id))
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

    /// Resolve the value expressions of one source-ordered component modifier.
    ///
    /// `Component::source_modifications` keeps the modifier list exactly as
    /// written (order, `each`/`final`/`redeclare` prefixes) alongside the
    /// keyed `Component::modifications`. The read-only AST walkers prefer the
    /// source-ordered list whenever it is present, so every later consumer that
    /// needs exact identities — strict-reachability dependency collection, the
    /// formatter, redeclare validation — reads *this* copy. Leaving it
    /// unresolved would hide the references it contains from those consumers.
    ///
    /// Modifier *targets* name members of the modified component's declared
    /// type, which Resolve does not own (instantiation applies redeclares
    /// first), so only the values are resolved here, in the enclosing class
    /// scope where they are written (MLS §7.2.5).
    fn resolve_source_modification(&mut self, expr: &mut Expression, class_scope: ScopeId) {
        match expr {
            Expression::Modification { value, .. } => {
                self.resolve_source_modification(std::sync::Arc::make_mut(value), class_scope)
            }
            Expression::ClassModification { modifications, .. } => {
                for modification in modifications.iter_mut() {
                    self.resolve_source_modification(modification, class_scope);
                }
            }
            Expression::NamedArgument { value, .. } => {
                self.resolve_expression(std::sync::Arc::make_mut(value), class_scope)
            }
            other => self.resolve_expression(other, class_scope),
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
    /// proceeds to enclosing scopes. Composite names receive an exact final
    /// identity when all traversed types are declaration-stable. A missing
    /// static tail is an immediate error; only instance-dependent type edges
    /// are deferred to instantiation.
    pub(crate) fn resolve_component_reference(
        &mut self,
        comp: &mut ComponentReference,
        scope: ScopeId,
    ) {
        if comp.parts.is_empty() {
            return;
        }
        for part in &mut comp.parts {
            part.def_id = None;
        }

        // Get the first part of the reference
        let first_name = &comp.parts[0].ident.text;

        // Look up the name in the scope tree
        if let Some(def_id) = self
            .scope_tree
            .lookup(scope, &ComponentPath::from_flat_path(first_name))
        {
            comp.parts[0].def_id = Some(def_id);
            self.stats.comp_refs_resolved += 1;
        } else {
            self.stats.comp_refs_unresolved += 1;
        }
        match self.resolve_component_reference_full_path(comp, scope) {
            FullPathResolution::Exact(_)
            | FullPathResolution::DeferredDynamic
            | FullPathResolution::UnresolvedRoot => {}
            FullPathResolution::MissingStaticTail => {
                self.emit_missing_static_tail(comp);
            }
        }
        // Also resolve subscript expressions
        for part in comp.parts.iter_mut() {
            if let Some(subs) = &mut part.subs {
                self.resolve_subscripts(subs, scope);
            }
        }
    }

    /// Prove the declaration identity of the member projected by `base.field`.
    ///
    /// MLS §3.7.3 writes a projection of a parenthesized primary (`(r).re`,
    /// `(if c then a else b).re`) as a postfix `.ident`, which the AST keeps as
    /// `Expression::FieldAccess` rather than folding into the base component
    /// reference. The projected member is a declaration exactly like the tail of
    /// `r.re`, so it is proved the same way: against the scope of the base's
    /// declared type. Only a base whose own identity Resolve holds is answered;
    /// a base whose type is instance-dependent (replaceable edge, expandable
    /// connector) or which no declaration owns (a function result) keeps its
    /// absent identity for the phase that can prove it, and reaches the Flat
    /// boundary as EF024 if nothing does.
    fn resolve_field_access_member(&self, base: &Expression, field: &str) -> Option<DefId> {
        let base_def_id = base_declaration_identity(base)?;
        let container = self
            .component_type_def_ids
            .get(&base_def_id)
            .copied()
            .unwrap_or(base_def_id);
        if self.dynamic_member_root_ids.contains(&base_def_id)
            || self.dynamic_member_root_ids.contains(&container)
        {
            return None;
        }
        let container_scope = self.class_def_scopes.get(&container).copied()?;
        self.scope_tree
            .lookup_member(container_scope, &ComponentPath::from_flat_path(field))
    }

    /// Resolve a function reference to its callable DefId while preserving the
    /// source component-reference parts for later scope-sensitive phases.
    ///
    /// Unlike generic component references, function calls should resolve the
    /// entire path (including inherited package members) at resolve time so
    /// later phases do exact function lookup without name heuristics.
    fn resolve_function_reference(&mut self, comp: &mut ComponentReference, scope: ScopeId) {
        self.resolve_component_reference(comp, scope);
        self.reject_non_callable_callee_capture(comp);

        // A root identity is not proof that the called member exists. Static
        // calls carry an identity on every part; dynamic tails remain absent
        // until instantiation proves them.
    }

    /// Undo a callee capture by a declaration that cannot be called.
    ///
    /// MLS §12.4.1 admits exactly two spellings in call position: a class
    /// (function; also a record constructor per §12.6 and a `type` conversion
    /// per §4.8.5.2) and a component of a *function* type — a functional input
    /// argument (MLS §12.4.2). A component of any other type is not callable,
    /// so it does not participate in call-position lookup and therefore cannot
    /// shadow a predefined operator of the same spelling (MLS §3.7); the
    /// component and the operator are distinguished by the syntactic position,
    /// not by declaration order. `model M input Real sample[3]; ... when
    /// sample(0.0, dt)` is the canonical case: the reads name the component,
    /// the call names the operator.
    ///
    /// Ordinary lookup (MLS §5.3) cannot make that distinction because it is
    /// position-blind, so this is the call-position refinement of its result.
    /// SPEC_0008 makes Resolve the first owner of name binding, so the
    /// correction belongs here rather than in a later phase re-deriving the
    /// callee from its spelling: the reference leaves Resolve carrying the
    /// exact predefined `DefId`, which is what the typed predefined-operator
    /// lowering matches on.
    fn reject_non_callable_callee_capture(&mut self, comp: &mut ComponentReference) {
        // Predefined operators are simple names (MLS §3.7); a composite callee
        // never denotes one, so its capture is not a shadowing question.
        if comp.parts.len() != 1 {
            return;
        }
        let Some(captured) = comp.parts[0].def_id else {
            return;
        };
        if self.declaration_is_callable(captured) {
            return;
        }
        let root_path = ComponentPath::from_parts([comp.parts[0].ident.text.as_ref()]);
        if let Some(predefined) = self.scope_tree.predefined_member(&root_path) {
            comp.parts[0].def_id = Some(predefined);
        }
    }

    /// True when `declaration` may stand in call position (MLS §12.4.1).
    ///
    /// A predefined declaration always may — that is the operator itself. A
    /// class always may: a function is called, a record builds its constructor
    /// (MLS §12.6) and a `type` converts (MLS §4.8.5.2). A component may only
    /// when its declared type is a function class, which is the functional
    /// input argument of MLS §12.4.2.
    ///
    /// A declaration whose declared type Resolve has not proved yet keeps its
    /// binding: absence of a proof is not proof of a non-callable capture, and
    /// Instantiate owns the deferred type edges.
    fn declaration_is_callable(&self, declaration: DefId) -> bool {
        if self.is_builtin(declaration) || self.class_types.contains_key(&declaration) {
            return true;
        }
        let Some(declared_type) = self.component_type_def_ids.get(&declaration) else {
            return true;
        };
        match self.class_types.get(declared_type) {
            Some(rumoca_core::ClassType::Function) => true,
            Some(_) => false,
            // A builtin declared type (`Real`, `Integer`, …) is proved by its
            // reserved identity rather than by a class-type entry, and it is
            // never a function type.
            None => !self.is_builtin(*declared_type),
        }
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
        comp: &mut ComponentReference,
        scope: ScopeId,
    ) -> FullPathResolution {
        let Some(first_part) = comp.parts.first() else {
            return FullPathResolution::UnresolvedRoot;
        };
        let Some(mut current_def_id) = comp
            .root_def_id()
            .or_else(|| self.resolve_function_first_part(&first_part.ident.text, scope))
        else {
            return FullPathResolution::UnresolvedRoot;
        };
        let root_path = ComponentPath::from_parts([first_part.ident.text.as_ref()]);
        if self.scope_tree.predefined_member(&root_path) == Some(current_def_id) {
            return self.resolve_predefined_reference_tail(comp, current_def_id);
        }

        for index in 1..comp.parts.len() {
            if self.dynamic_member_root_ids.contains(&current_def_id) {
                return FullPathResolution::DeferredDynamic;
            }
            let container = self
                .component_type_def_ids
                .get(&current_def_id)
                .copied()
                .unwrap_or(current_def_id);
            if self.dynamic_member_root_ids.contains(&container) {
                return FullPathResolution::DeferredDynamic;
            }
            let Some(container_scope) = self.class_def_scopes.get(&container).copied() else {
                return FullPathResolution::MissingStaticTail;
            };
            let part = &comp.parts[index];
            let Some(member) = self.scope_tree.lookup_member(
                container_scope,
                &ComponentPath::from_flat_path(&part.ident.text),
            ) else {
                return self.absent_member_resolution(container);
            };
            comp.parts[index].def_id = Some(member);
            current_def_id = member;
        }

        FullPathResolution::Exact(current_def_id)
    }

    /// Classify a qualified tail whose member is absent from `container`.
    ///
    /// MLS §9.1.3: an expandable connector acquires members from the `connect`
    /// equations that name them, so an absent member is instance-dependent
    /// rather than statically missing. Flatten proves whether the member is
    /// ever supplied (EF020). Every other container has a closed member set,
    /// so the tail is rejected here.
    fn absent_member_resolution(&self, container: DefId) -> FullPathResolution {
        if self.expandable_connector_ids.contains(&container) {
            return FullPathResolution::DeferredDynamic;
        }
        FullPathResolution::MissingStaticTail
    }

    fn resolve_predefined_reference_tail(
        &self,
        comp: &mut ComponentReference,
        mut current_def_id: DefId,
    ) -> FullPathResolution {
        for index in 1..comp.parts.len() {
            let path = ComponentPath::from_parts(
                comp.parts
                    .iter()
                    .take(index + 1)
                    .map(|part| part.ident.text.as_ref()),
            );
            let Some(member) = self.scope_tree.predefined_member(&path) else {
                return FullPathResolution::MissingStaticTail;
            };
            comp.parts[index].def_id = Some(member);
            current_def_id = member;
        }
        FullPathResolution::Exact(current_def_id)
    }

    fn emit_missing_static_tail(&mut self, comp: &ComponentReference) {
        let primary_label =
            PrimaryLabel::new(comp.span).with_message("unresolved component reference");
        self.diagnostics.emit(Diagnostic::error(
            "ER002",
            format!("unresolved component reference: '{comp}'"),
            primary_label,
        ));
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

/// The declaration identity a projection base denotes, when Resolve holds one.
///
/// Parentheses and array indexing do not change which declaration a path-shaped
/// primary denotes (MLS §3.7.3), so both are traversed. Every other expression
/// form denotes a value with no declaration of its own.
fn base_declaration_identity(base: &Expression) -> Option<DefId> {
    let mut current = base;
    loop {
        match current {
            Expression::Parenthesized { inner, .. } => current = inner,
            Expression::ArrayIndex { base, .. } => current = base,
            Expression::ComponentReference(reference) => return reference.target_def_id(),
            Expression::FieldAccess { field_def_id, .. } => return *field_def_id,
            _ => return None,
        }
    }
}
