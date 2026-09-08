//! Phase 2a: Extends Resolution - resolve all extends clauses first.
//!
//! This phase processes extends clauses in breadth-first order by nesting depth,
//! ensuring inheritance edges are complete before nested class resolution.

use crate::Resolver;
use rumoca_core::{ComponentPath, DefId, ScopeId};
use rumoca_core::{Diagnostic, PrimaryLabel};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;

enum ImportPathResolution {
    /// The path resolved: `prefix` holds the identities of the containing
    /// segments (outermost first, excluding the target) and `target` the
    /// imported definition itself.
    Found {
        prefix: Vec<DefId>,
        target: DefId,
    },
    Absent,
    AmbiguousInherited,
    AmbiguousUnqualifiedImport,
}

impl Resolver {
    /// Resolve all imports and extends clauses level-by-level (Phase 2a).
    ///
    /// This processes extends clauses in breadth-first order by nesting depth:
    /// 1. First, resolve extends for ALL top-level classes
    /// 2. Then, resolve extends for ALL depth-1 nested classes
    /// 3. Continue until no more nested classes
    ///
    /// This ensures that when resolving extends for a nested class that needs
    /// inherited member lookup, all sibling classes at the same or higher level
    /// have had their extends resolved, making their inheritance edges available.
    pub(crate) fn resolve_extends_all(
        &mut self,
        def: &mut ast::StoredDefinition,
        prefix: &str,
        emit_errors: bool,
    ) {
        // Process level by level using recursive depth-limited traversal
        let max_depth = self.compute_max_nesting_depth_stored(def);

        for depth in 0..=max_depth {
            self.resolve_extends_at_depth(def, prefix, 0, depth, emit_errors);
        }
    }

    /// Resolve every class's imports as one idempotent scope update.
    pub(crate) fn resolve_imports_all(&mut self, def: &ast::StoredDefinition, emit_errors: bool) {
        for class in def.classes.values() {
            self.resolve_class_imports(class, emit_errors);
        }
    }

    fn resolve_class_imports(&mut self, class: &ast::ClassDef, emit_errors: bool) {
        let scope = class
            .scope_id
            .expect("class scope must be constructed during registration");
        let imports = class
            .imports
            .iter()
            .filter_map(|import| self.resolve_import(import, emit_errors))
            .flatten()
            .collect();
        self.scope_tree.set_imports(scope, imports);
        for nested in class.classes.values() {
            self.resolve_class_imports(nested, emit_errors);
        }
    }

    /// Compute maximum nesting depth of classes in a ast::StoredDefinition.
    fn compute_max_nesting_depth_stored(&self, def: &ast::StoredDefinition) -> usize {
        def.classes
            .values()
            .map(|c| self.compute_max_nesting_depth_class(c))
            .max()
            .unwrap_or(0)
    }

    /// Compute maximum nesting depth of a class and its nested classes.
    fn compute_max_nesting_depth_class(&self, class: &ast::ClassDef) -> usize {
        if class.classes.is_empty() {
            0
        } else {
            1 + class
                .classes
                .values()
                .map(|c| self.compute_max_nesting_depth_class(c))
                .max()
                .unwrap_or(0)
        }
    }

    /// Resolve extends for all classes at a specific depth.
    fn resolve_extends_at_depth(
        &mut self,
        def: &mut ast::StoredDefinition,
        prefix: &str,
        current_depth: usize,
        target_depth: usize,
        emit_errors: bool,
    ) {
        for (name, class) in def.classes.iter_mut() {
            let qualified_name = if prefix.is_empty() {
                name.clone()
            } else {
                format!("{}.{}", prefix, name)
            };
            self.resolve_extends_class_at_depth(
                class,
                &qualified_name,
                current_depth,
                target_depth,
                emit_errors,
            );
        }
    }

    /// Resolve extends for a class if at target depth, or recurse to nested classes.
    fn resolve_extends_class_at_depth(
        &mut self,
        class: &mut ast::ClassDef,
        qualified_name: &str,
        current_depth: usize,
        target_depth: usize,
        emit_errors: bool,
    ) {
        if current_depth == target_depth {
            // At target depth - resolve imports and extends for this class
            self.resolve_extends_single(class, qualified_name, emit_errors);
        } else if current_depth < target_depth {
            // Not deep enough yet - recurse into nested classes
            for (nested_name, nested) in class.classes.iter_mut() {
                let nested_qualified = format!("{}.{}", qualified_name, nested_name);
                self.resolve_extends_class_at_depth(
                    nested,
                    &nested_qualified,
                    current_depth + 1,
                    target_depth,
                    emit_errors,
                );
            }
        }
        // If current_depth > target_depth, we've gone too deep - do nothing
    }

    /// Resolve imports and extends for a single class (no recursion).
    fn resolve_extends_single(
        &mut self,
        class: &mut ast::ClassDef,
        qualified_name: &str,
        emit_errors: bool,
    ) {
        let class_scope = class
            .scope_id
            .expect("Class scope should be set in registration phase");
        let class_def_id = class
            .def_id
            .expect("Class DefId should be set in registration phase");

        // Add this class to the resolving set for circular inheritance detection
        self.resolving_extends.insert(class_def_id);

        // Resolve extends clauses (MLS §7.1)
        // Use class_scope so that class-local imports (like `import D = Package`) are visible.
        // The `exclude` parameter in resolve_qualified_name_excluding handles self-references
        // (e.g., `record ThermodynamicState extends ThermodynamicState` won't find itself).
        for extend in class.extends.iter_mut() {
            self.resolve_extends(
                extend,
                class_scope,
                qualified_name,
                class_def_id,
                emit_errors,
            );
        }

        if class.is_redeclare {
            let class_name = class.name.text.as_ref();
            let class_extends_target = class.extends.iter().find_map(|extend| {
                let extends_same_slot = extend
                    .base_name
                    .name
                    .last()
                    .is_some_and(|part| part.text.as_ref() == class_name);
                extends_same_slot.then_some(extend.base_def_id).flatten()
            });
            // MLS §5.3.2 / §7.3: a `redeclare` element without an `extends`
            // clause of its own replaces the same-named element inherited by
            // the *enclosing* class. That enclosing class is the owner of the
            // parent scope (SPEC_0002), addressed by its `DefId` (SPEC_0001).
            let inherited_target = self.resolve_redeclare_target(
                class_def_id,
                class_name,
                &class.name.location,
                emit_errors,
            );
            class.redeclare_target_def_id = class_extends_target
                .or(inherited_target)
                .filter(|target| *target != class_def_id);
        }

        self.resolving_extends.remove(&class_def_id);
    }

    fn resolve_redeclare_target(
        &mut self,
        class_def_id: DefId,
        class_name: &str,
        location: &rumoca_core::Location,
        emit_errors: bool,
    ) -> Option<DefId> {
        let container = self.enclosing_class_def_id(class_def_id)?;
        match self.lookup_inherited_class_member(container, class_name) {
            ast::LookupOutcome::Found(definition) => Some(definition),
            ast::LookupOutcome::Absent => None,
            ast::LookupOutcome::AmbiguousInherited => {
                self.note_ambiguous_inherited_name(
                    location,
                    class_name,
                    "redeclare target",
                    emit_errors,
                );
                None
            }
            ast::LookupOutcome::AmbiguousUnqualifiedImport => {
                self.note_ambiguous_unqualified_name(
                    location,
                    class_name,
                    "redeclare target",
                    emit_errors,
                );
                None
            }
        }
    }

    /// Resolve an extends clause (MLS §7.1).
    ///
    /// Looks up the base class name in the scope tree and sets base_def_id.
    /// Records the inheritance edge for later cycle detection.
    /// Also checks for direct circular inheritance (A extends A).
    pub(crate) fn resolve_extends(
        &mut self,
        extend: &mut ast::Extend,
        scope: ScopeId,
        class_name: &str,
        current_class_def_id: DefId,
        emit_errors: bool,
    ) {
        if extend.base_def_id.is_some() {
            return;
        }
        let base_name = &extend.base_name;

        // Handle qualified names (e.g., "Package.SubPackage.Model")
        if base_name.name.is_empty() {
            return;
        }

        // Try to resolve the base class name with exclusion.
        // Exclusion handles "redeclare extends SameName" pattern (MLS §7.3) where a nested
        // class extends an inherited class with the same short name. Without exclusion,
        // the class would find itself and trigger a false circular inheritance error.
        //
        // Direct self-extension (model A extends A) and indirect cycles (A→B→A) are
        // detected by the `resolving_extends` set and Phase 3 cycle detection.
        let def_id =
            self.resolve_qualified_name_excluding(base_name, scope, Some(current_class_def_id));

        match def_id {
            ast::LookupOutcome::Found(base_def_id) => self.record_resolved_base(
                extend,
                class_name,
                current_class_def_id,
                base_def_id,
                emit_errors,
            ),
            ast::LookupOutcome::Absent => {
                if emit_errors {
                    self.emit_base_class_not_found(&extend.location, &extend.base_name);
                    self.stats.extends_unresolved += 1;
                }
            }
            ast::LookupOutcome::AmbiguousInherited => {
                self.note_ambiguous_inherited_name(
                    &extend.location,
                    &extend.base_name.to_string(),
                    "extends clause",
                    emit_errors,
                );
                if emit_errors {
                    self.stats.extends_unresolved += 1;
                }
            }
            ast::LookupOutcome::AmbiguousUnqualifiedImport => {
                self.note_ambiguous_unqualified_name(
                    &extend.location,
                    &extend.base_name.to_string(),
                    "extends clause",
                    emit_errors,
                );
                if emit_errors {
                    self.stats.extends_unresolved += 1;
                }
            }
        }
    }

    /// Bind `extend` to an already-resolved base class, unless that base is
    /// itself mid-resolution and would close an inheritance cycle.
    fn record_resolved_base(
        &mut self,
        extend: &mut ast::Extend,
        class_name: &str,
        current_class_def_id: DefId,
        base_def_id: DefId,
        emit_errors: bool,
    ) {
        // Catches indirect cycles like: model A extends B; model B extends A;
        if self.resolving_extends.contains(&base_def_id) {
            if emit_errors {
                self.emit_circular_extends(&extend.location, class_name, &extend.base_name);
                self.stats.extends_unresolved += 1;
            }
            return;
        }
        extend.base_def_id = Some(base_def_id);
        // Record edge for Phase 3 cycle detection and O(1) lookup
        self.add_inheritance_edge(current_class_def_id, base_def_id, extend.location.clone());
        self.stats.extends_resolved += 1;
    }

    fn emit_circular_extends(
        &mut self,
        location: &rumoca_core::Location,
        class_name: &str,
        base_name: impl std::fmt::Display,
    ) {
        let Some(span) = crate::location_span_or_emit(
            &mut self.diagnostics,
            location,
            &self.source_map,
            "extends clause",
        ) else {
            return;
        };
        self.diagnostics.emit(Diagnostic::error(
            "ER004",
            format!(
                "circular inheritance: `{}` extends `{}` which creates a cycle",
                class_name, base_name
            ),
            PrimaryLabel::new(span).with_message("circular extends chain detected"),
        ));
    }

    fn emit_base_class_not_found(
        &mut self,
        location: &rumoca_core::Location,
        base_name: impl std::fmt::Display,
    ) {
        let Some(span) = crate::location_span_or_emit(
            &mut self.diagnostics,
            location,
            &self.source_map,
            "extends clause",
        ) else {
            return;
        };
        self.diagnostics.emit(Diagnostic::error(
            "ER003",
            format!("base class not found: `{}` does not exist", base_name),
            PrimaryLabel::new(span).with_message("base class not found"),
        ));
    }

    fn note_ambiguous_inherited_name(
        &mut self,
        location: &rumoca_core::Location,
        name: &str,
        context: &str,
        emit_errors: bool,
    ) {
        if !emit_errors {
            return;
        }
        let Some(span) = crate::location_span_or_emit(
            &mut self.diagnostics,
            location,
            &self.source_map,
            context,
        ) else {
            return;
        };
        self.emit_ambiguous_inherited_lookup(name, span);
    }

    fn note_ambiguous_unqualified_name(
        &mut self,
        location: &rumoca_core::Location,
        name: &str,
        context: &str,
        emit_errors: bool,
    ) {
        if !emit_errors {
            return;
        }
        let Some(span) = crate::location_span_or_emit(
            &mut self.diagnostics,
            location,
            &self.source_map,
            context,
        ) else {
            return;
        };
        self.emit_ambiguous_unqualified_import(name, span);
    }

    /// Report an import whose path did not resolve, when diagnostics are enabled.
    ///
    /// The `emit_errors` check lives here so the call sites stay flat: this
    /// runs inside per-variant `let`-`else` arms that are already three levels
    /// deep (SPEC_0021 nesting budget).
    fn note_unresolved_import(&mut self, import: &ast::Import, emit_errors: bool) {
        if emit_errors {
            self.emit_unresolved_import(import);
        }
    }

    /// Report an import that resolved to something that cannot be imported.
    fn note_invalid_import_target(&mut self, import: &ast::Import, emit_errors: bool) {
        if emit_errors {
            self.emit_invalid_import_target(import);
        }
    }

    /// Resolve an import clause (MLS §13.2).
    ///
    /// Converts an AST import to a scope import carrying resolved identities.
    /// Returns None if resolution fails.
    pub(crate) fn resolve_import(
        &mut self,
        import: &ast::Import,
        emit_errors: bool,
    ) -> Option<Vec<ast::scope::Import>> {
        let scope_imports = match import {
            ast::Import::Qualified { path, .. } => {
                // import A.B.C; -> makes C available as C
                let (prefix, def_id) =
                    self.resolve_import_path_or_emit(import, path, emit_errors)?;
                if !self.qualified_import_target_is_valid(&prefix, def_id) {
                    self.note_invalid_import_target(import, emit_errors);
                    return None;
                }
                let imported_name = path.name.last()?;
                vec![ast::scope::Import::SingleDefinition {
                    name: ComponentPath::from_flat_path(&imported_name.text),
                    prefix,
                    def_id,
                }]
            }
            ast::Import::Renamed { alias, path, .. } => {
                // import D = A.B.C; -> makes C available as D
                let (prefix, def_id) =
                    self.resolve_import_path_or_emit(import, path, emit_errors)?;
                if !self.qualified_import_target_is_valid(&prefix, def_id) {
                    self.note_invalid_import_target(import, emit_errors);
                    return None;
                }
                vec![ast::scope::Import::SingleDefinition {
                    name: ComponentPath::from_flat_path(&alias.text),
                    prefix,
                    def_id,
                }]
            }
            ast::Import::Unqualified { path, .. } => {
                // import A.B.*; -> imports the package member snapshot
                let (mut prefix, pkg_def_id) =
                    self.resolve_import_path_or_emit(import, path, emit_errors)?;
                if !self.package_import_target_is_valid(&prefix, pkg_def_id) {
                    self.note_invalid_import_target(import, emit_errors);
                    return None;
                }
                let names = self.collect_package_children(pkg_def_id);
                prefix.push(pkg_def_id);
                vec![ast::scope::Import::Wildcard { prefix, names }]
            }
            ast::Import::Selective { path, names, .. } => {
                // import A.B.{C, D}; -> imports specific names from A.B
                let (mut prefix, pkg_def_id) =
                    self.resolve_import_path_or_emit(import, path, emit_errors)?;
                if !self.package_import_target_is_valid(&prefix, pkg_def_id) {
                    self.note_invalid_import_target(import, emit_errors);
                    return None;
                }
                let resolved_names =
                    self.resolve_selective_import_entries(import, pkg_def_id, names, emit_errors)?;
                prefix.push(pkg_def_id);
                resolved_names
                    .into_iter()
                    .map(|(name, def_id)| ast::scope::Import::SingleDefinition {
                        name,
                        prefix: prefix.clone(),
                        def_id,
                    })
                    .collect()
            }
        };

        Some(scope_imports)
    }

    fn resolve_import_path_or_emit(
        &mut self,
        import: &ast::Import,
        path: &ast::Name,
        emit_errors: bool,
    ) -> Option<(Vec<DefId>, DefId)> {
        match self.resolve_import_path(path) {
            ImportPathResolution::Found { prefix, target } => Some((prefix, target)),
            ImportPathResolution::Absent => {
                self.note_unresolved_import(import, emit_errors);
                None
            }
            ImportPathResolution::AmbiguousInherited => {
                self.note_ambiguous_inherited_name(
                    import.location(),
                    &path.to_string(),
                    "import clause",
                    emit_errors,
                );
                None
            }
            ImportPathResolution::AmbiguousUnqualifiedImport => {
                self.note_ambiguous_unqualified_name(
                    import.location(),
                    &path.to_string(),
                    "import clause",
                    emit_errors,
                );
                None
            }
        }
    }

    fn resolve_import_path(&self, path: &ast::Name) -> ImportPathResolution {
        if path.name.is_empty() {
            return ImportPathResolution::Absent;
        }

        let first_part = &path.name[0].text;
        let first_path = ComponentPath::from_flat_path(first_part);
        // MLS §13.2.2: unlike ordinary lexical lookup, every import path
        // starts by resolving its first segment in the top-level scope.
        let Some(mut current_def_id) = self.scope_tree.lookup_local(ScopeId::GLOBAL, &first_path)
        else {
            return ImportPathResolution::Absent;
        };
        let mut prefix = Vec::new();

        for part in path.name.iter().skip(1) {
            prefix.push(current_def_id);
            current_def_id = match self.lookup_class_member(current_def_id, &part.text) {
                ast::LookupOutcome::Found(definition) => definition,
                ast::LookupOutcome::Absent => return ImportPathResolution::Absent,
                ast::LookupOutcome::AmbiguousInherited => {
                    return ImportPathResolution::AmbiguousInherited;
                }
                ast::LookupOutcome::AmbiguousUnqualifiedImport => {
                    return ImportPathResolution::AmbiguousUnqualifiedImport;
                }
            };
        }

        ImportPathResolution::Found {
            prefix,
            target: current_def_id,
        }
    }

    fn qualified_import_target_is_valid(&self, prefix: &[DefId], target: DefId) -> bool {
        if prefix.is_empty() {
            // MLS §13.2: a single-segment import may name a class directly
            // (for example `import Complex;` inside an operator record).
            return self.class_types.contains_key(&target);
        }
        prefix
            .iter()
            .copied()
            .all(|def_id| self.is_package_def(def_id))
    }

    fn package_import_target_is_valid(&self, prefix: &[DefId], target: DefId) -> bool {
        self.qualified_import_target_is_valid(prefix, target) && self.is_package_def(target)
    }

    fn is_package_def(&self, def_id: DefId) -> bool {
        self.class_types.get(&def_id) == Some(&rumoca_core::ClassType::Package)
    }

    fn emit_unresolved_import(&mut self, import: &ast::Import) {
        let Some(span) = self.import_span(import, "import clause") else {
            return;
        };
        self.diagnostics.emit(Diagnostic::error(
            "ER002",
            format!(
                "unresolved import: '{}'",
                Self::format_import_clause(import)
            ),
            PrimaryLabel::new(span).with_message("import could not be resolved"),
        ));
    }

    fn emit_invalid_import_target(&mut self, import: &ast::Import) {
        let Some(span) = self.import_span(import, "import clause") else {
            return;
        };
        self.diagnostics.emit(Diagnostic::error(
            "ER002",
            format!(
                "invalid import target: '{}' (imports must traverse packages only)",
                Self::format_import_clause(import)
            ),
            PrimaryLabel::new(span).with_message("import target is not a package path"),
        ));
    }

    fn emit_unresolved_selective_import_member(
        &mut self,
        import: &ast::Import,
        name_token: &rumoca_core::Token,
    ) {
        let Some(span) = crate::location_span_or_emit(
            &mut self.diagnostics,
            &name_token.location,
            &self.source_map,
            "selective import member",
        ) else {
            return;
        };
        self.diagnostics.emit(Diagnostic::error(
            "ER002",
            format!(
                "unresolved import member: '{}' in '{}'",
                name_token.text,
                Self::format_import_clause(import)
            ),
            PrimaryLabel::new(span).with_message("import member could not be resolved"),
        ));
    }

    /// Report a selective-import member that the package does not declare.
    ///
    /// Gated here rather than at the call site, which sits inside a loop and a
    /// branch already at the SPEC_0021 nesting budget.
    fn note_unresolved_selective_import_member(
        &mut self,
        import: &ast::Import,
        name_token: &rumoca_core::Token,
        emit_errors: bool,
    ) {
        if emit_errors {
            self.emit_unresolved_selective_import_member(import, name_token);
        }
    }

    fn resolve_selective_import_entries(
        &mut self,
        import: &ast::Import,
        package: DefId,
        names: &[rumoca_core::Token],
        emit_errors: bool,
    ) -> Option<IndexMap<ComponentPath, DefId>> {
        let mut resolved_names = IndexMap::default();
        let mut has_missing_name = false;
        for name_token in names {
            match self.lookup_class_member(package, &name_token.text) {
                ast::LookupOutcome::Found(definition) => {
                    resolved_names
                        .insert(ComponentPath::from_flat_path(&name_token.text), definition);
                }
                ast::LookupOutcome::Absent => {
                    has_missing_name = true;
                    self.note_unresolved_selective_import_member(import, name_token, emit_errors);
                }
                ast::LookupOutcome::AmbiguousInherited => {
                    has_missing_name = true;
                    self.note_ambiguous_inherited_name(
                        &name_token.location,
                        &name_token.text,
                        "selective import member",
                        emit_errors,
                    );
                }
                ast::LookupOutcome::AmbiguousUnqualifiedImport => {
                    has_missing_name = true;
                    self.note_ambiguous_unqualified_name(
                        &name_token.location,
                        &name_token.text,
                        "selective import member",
                        emit_errors,
                    );
                }
            }
        }
        if has_missing_name {
            None
        } else {
            Some(resolved_names)
        }
    }

    fn import_span(&mut self, import: &ast::Import, context: &str) -> Option<rumoca_core::Span> {
        crate::location_span_or_emit(
            &mut self.diagnostics,
            import.location(),
            &self.source_map,
            context,
        )
    }

    fn format_import_clause(import: &ast::Import) -> String {
        fn format_path(path: &ast::Name, global_scope: bool) -> String {
            if global_scope {
                format!(".{path}")
            } else {
                path.to_string()
            }
        }

        match import {
            ast::Import::Qualified {
                path, global_scope, ..
            } => format_path(path, *global_scope),
            ast::Import::Renamed {
                alias,
                path,
                global_scope,
                ..
            } => format!("{} = {}", alias.text, format_path(path, *global_scope)),
            ast::Import::Unqualified {
                path, global_scope, ..
            } => format!("{}.*", format_path(path, *global_scope)),
            ast::Import::Selective {
                path,
                names,
                global_scope,
                ..
            } => {
                let names = names
                    .iter()
                    .map(|name| name.text.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}.{{{names}}}", format_path(path, *global_scope))
            }
        }
    }

    /// Collect the authoritative direct-and-inherited member view of a package.
    fn collect_package_children(
        &self,
        package: DefId,
    ) -> IndexMap<ComponentPath, ast::WildcardMember> {
        let scope = self
            .class_def_scopes
            .get(&package)
            .copied()
            .expect("a resolved package declaration must own its registered class scope");
        self.scope_tree.importable_members(scope)
    }
}
