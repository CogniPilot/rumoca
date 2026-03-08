//! Phase 2a: Extends Resolution - resolve all extends clauses first.
//!
//! This phase processes extends clauses in breadth-first order by nesting depth,
//! ensuring inheritance edges are complete before nested class resolution.

use crate::Resolver;
use indexmap::IndexMap;
use rumoca_core::{DefId, Diagnostic, Label, ScopeId};
use rumoca_ir_ast as ast;

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
    pub(crate) fn resolve_extends_all(&mut self, def: &mut ast::StoredDefinition, prefix: &str) {
        // Process level by level using recursive depth-limited traversal
        let max_depth = self.compute_max_nesting_depth_stored(def);

        for depth in 0..=max_depth {
            self.resolve_extends_at_depth(def, prefix, 0, depth);
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
    ) {
        if current_depth == target_depth {
            // At target depth - resolve imports and extends for this class
            self.resolve_extends_single(class, qualified_name);
        } else if current_depth < target_depth {
            // Not deep enough yet - recurse into nested classes
            for (nested_name, nested) in class.classes.iter_mut() {
                let nested_qualified = format!("{}.{}", qualified_name, nested_name);
                self.resolve_extends_class_at_depth(
                    nested,
                    &nested_qualified,
                    current_depth + 1,
                    target_depth,
                );
            }
        }
        // If current_depth > target_depth, we've gone too deep - do nothing
    }

    /// Resolve imports and extends for a single class (no recursion).
    fn resolve_extends_single(&mut self, class: &mut ast::ClassDef, qualified_name: &str) {
        let class_scope = class
            .scope_id
            .expect("Class scope should be set in registration phase");
        let class_def_id = class
            .def_id
            .expect("Class DefId should be set in registration phase");

        // Resolve imports first (MLS §13.2) - they may be needed for extends resolution
        for import in &class.imports {
            self.resolve_import(import, class_scope);
        }

        // Add this class to the resolving set for circular inheritance detection
        self.resolving_extends.insert(class_def_id);

        // Resolve extends clauses (MLS §7.1)
        // Use class_scope so that class-local imports (like `import D = Package`) are visible.
        // The `exclude` parameter in resolve_qualified_name_excluding handles self-references
        // (e.g., `record ThermodynamicState extends ThermodynamicState` won't find itself).
        for extend in class.extends.iter_mut() {
            self.resolve_extends(extend, class_scope, qualified_name, class_def_id);
        }

        // Remove from resolving set after extends are processed
        self.resolving_extends.remove(&class_def_id);
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
    ) {
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
            Some(base_def_id) => {
                // Check if this base class is part of an inheritance chain being resolved.
                // This catches indirect cycles like: model A extends B; model B extends A;
                if self.resolving_extends.contains(&base_def_id) {
                    let span = crate::location_to_span(&extend.location, &self.source_map);
                    self.diagnostics.emit(
                        Diagnostic::error(format!(
                            "circular inheritance: `{}` extends `{}` which creates a cycle",
                            class_name, base_name
                        ))
                        .with_code("ER004")
                        .with_label(
                            Label::primary(span).with_message("circular extends chain detected"),
                        ),
                    );
                    self.stats.extends_unresolved += 1;
                } else {
                    extend.base_def_id = Some(base_def_id);
                    // Record edge for Phase 3 cycle detection and O(1) lookup
                    self.add_inheritance_edge(
                        current_class_def_id,
                        base_def_id,
                        extend.location.clone(),
                    );
                    self.stats.extends_resolved += 1;
                }
            }
            None => {
                // Normal lookup failed - try inherited member lookup for simple names
                if let Some(inherited_def_id) =
                    self.try_inherited_member_lookup(base_name, class_name)
                {
                    self.record_extends_result(
                        extend,
                        class_name,
                        current_class_def_id,
                        inherited_def_id,
                    );
                    self.stats.extends_inherited += 1;
                    return;
                }

                // Base class not found - emit diagnostic
                let span = crate::location_to_span(&extend.location, &self.source_map);
                self.diagnostics.emit(
                    Diagnostic::error(format!(
                        "base class not found: `{}` does not exist",
                        base_name
                    ))
                    .with_code("ER003")
                    .with_label(Label::primary(span).with_message("base class not found")),
                );
                self.stats.extends_unresolved += 1;
            }
        }
    }

    /// Try inherited member lookup for "redeclare extends SameName" pattern.
    ///
    /// MLS §7.3: When a nested class extends an INHERITED class with the same short name,
    /// we search the containing class's inheritance chain.
    ///
    /// Example:
    /// ```modelica
    /// package Base record State end State; end Base;
    /// package Derived extends Base
    ///     redeclare record extends State end State;  // State from Base
    /// end Derived;
    /// ```
    fn try_inherited_member_lookup(
        &self,
        base_name: &rumoca_ir_ast::Name,
        class_name: &str,
    ) -> Option<DefId> {
        // Only applies to simple (single-part) names
        if base_name.name.len() != 1 {
            return None;
        }

        let member_name = &base_name.name[0].text;

        // Get the containing class's qualified name (parent of current class)
        let dot_pos = class_name.rfind('.')?;
        let container_name = &class_name[..dot_pos];

        self.lookup_inherited_member(container_name, member_name)
    }

    /// Record a successful extends resolution, checking for cycles.
    fn record_extends_result(
        &mut self,
        extend: &mut ast::Extend,
        class_name: &str,
        current_class_def_id: DefId,
        base_def_id: DefId,
    ) {
        if self.resolving_extends.contains(&base_def_id) {
            let span = crate::location_to_span(&extend.location, &self.source_map);
            self.diagnostics.emit(
                Diagnostic::error(format!(
                    "circular inheritance: `{}` extends `{}` which creates a cycle",
                    class_name, extend.base_name
                ))
                .with_code("ER004")
                .with_label(Label::primary(span).with_message("circular extends chain detected")),
            );
        } else {
            extend.base_def_id = Some(base_def_id);
            self.add_inheritance_edge(current_class_def_id, base_def_id, extend.location.clone());
        }
    }

    /// Resolve an import clause (MLS §13.2).
    ///
    /// Converts AST `Import` to `scope::Import` with resolved DefIds,
    /// and adds it to the scope's imports list.
    /// Returns None if resolution fails.
    pub(crate) fn resolve_import(&mut self, import: &ast::Import, scope: ScopeId) -> Option<()> {
        // Determine the starting scope for resolution
        // For global scope imports (leading dot), start from global scope
        let resolve_scope = if import.is_global_scope() {
            ScopeId(0) // Global scope
        } else {
            scope
        };

        let scope_import = match import {
            ast::Import::Qualified { path, .. } => {
                // import A.B.C; -> makes C available as C
                let Some(def_id) = self.resolve_qualified_name(path, resolve_scope) else {
                    self.emit_unresolved_import(import);
                    return None;
                };
                let path_strs: Vec<String> = path.name.iter().map(|t| t.text.to_string()).collect();
                ast::scope::Import::Qualified {
                    path: path_strs,
                    def_id,
                }
            }
            ast::Import::Renamed { alias, path, .. } => {
                // import D = A.B.C; -> makes C available as D
                let Some(def_id) = self.resolve_qualified_name(path, resolve_scope) else {
                    self.emit_unresolved_import(import);
                    return None;
                };
                ast::scope::Import::Renamed {
                    alias: alias.text.to_string(),
                    path: path.name.iter().map(|t| t.text.to_string()).collect(),
                    def_id,
                }
            }
            ast::Import::Unqualified { path, .. } => {
                // import A.B.*; -> imports all public names from A.B
                let Some(pkg_def_id) = self.resolve_qualified_name(path, resolve_scope) else {
                    self.emit_unresolved_import(import);
                    return None;
                };
                let Some(pkg_qualified) = self.def_names.get(&pkg_def_id) else {
                    self.emit_unresolved_import(import);
                    return None;
                };
                let names = self.collect_package_children(pkg_qualified);
                ast::scope::Import::Unqualified {
                    path: path.name.iter().map(|t| t.text.to_string()).collect(),
                    names,
                }
            }
            ast::Import::Selective { path, names, .. } => {
                // import A.B.{C, D}; -> imports specific names from A.B
                let Some(pkg_def_id) = self.resolve_qualified_name(path, resolve_scope) else {
                    self.emit_unresolved_import(import);
                    return None;
                };
                let Some(pkg_qualified) = self.def_names.get(&pkg_def_id) else {
                    self.emit_unresolved_import(import);
                    return None;
                };
                let resolved_names = self.resolve_selective_names(pkg_qualified, names);
                ast::scope::Import::Unqualified {
                    path: path.name.iter().map(|t| t.text.to_string()).collect(),
                    names: resolved_names,
                }
            }
        };

        let Some(scope_node) = self.scope_tree.get_mut(scope) else {
            self.emit_unresolved_import(import);
            return None;
        };
        scope_node.imports.push(scope_import);
        Some(())
    }

    fn emit_unresolved_import(&mut self, import: &ast::Import) {
        let span = crate::location_to_span(import.location(), &self.source_map);
        self.diagnostics.emit(
            Diagnostic::error(format!(
                "unresolved import: '{}' at {}",
                Self::format_import_clause(import),
                import.location()
            ))
            .with_code("ER002")
            .with_label(Label::primary(span).with_message("import could not be resolved")),
        );
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

    /// Collect all direct children of a package.
    ///
    /// Uses O(1) lookup via pre-computed package_children map.
    fn collect_package_children(&self, pkg_qualified: &str) -> IndexMap<String, DefId> {
        self.package_children
            .get(pkg_qualified)
            .cloned()
            .unwrap_or_default()
    }

    /// Resolve specific names from a package for selective imports.
    fn resolve_selective_names(
        &self,
        pkg_qualified: &str,
        names: &[rumoca_ir_core::Token],
    ) -> IndexMap<String, DefId> {
        let mut resolved_names = IndexMap::new();
        for name_token in names {
            let full_name = format!("{}.{}", pkg_qualified, name_token.text);
            if let Some(&def_id) = self.name_to_def.get(&full_name) {
                resolved_names.insert(name_token.text.to_string(), def_id);
            }
        }
        resolved_names
    }
}
