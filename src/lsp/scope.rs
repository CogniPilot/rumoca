//! Unified scope resolver for Modelica workspace.
//!
//! This module provides a workspace-aware scope resolver that handles:
//! - Local symbol resolution within a file
//! - Cross-file resolution using the workspace index
//! - `within` clause scope handling
//! - Import alias resolution
//! - Inheritance chain resolution across files

use crate::ir::ast::{ClassDefinition, Component, Import, StoredDefinition};
use crate::lsp::workspace::{WorkspaceState, WorkspaceSymbol};

/// A resolved symbol with its origin information
#[derive(Debug, Clone)]
pub enum ResolvedSymbol<'a> {
    /// A component (variable, parameter, etc.) resolved locally
    LocalComponent {
        component: &'a Component,
        /// The class where this component is defined
        defined_in: &'a ClassDefinition,
        /// If inherited, the name of the base class it came from
        inherited_via: Option<String>,
    },
    /// A class definition resolved locally
    LocalClass(&'a ClassDefinition),
    /// A symbol resolved from the workspace (cross-file)
    WorkspaceSymbol(&'a WorkspaceSymbol),
}

/// Workspace-aware scope resolver.
///
/// Combines local AST resolution with workspace-wide symbol lookup.
pub struct WorkspaceScopeResolver<'a> {
    /// The local AST for this file
    ast: &'a StoredDefinition,
    /// The workspace state for cross-file lookups
    workspace: &'a WorkspaceState,
}

impl<'a> WorkspaceScopeResolver<'a> {
    /// Create a new workspace scope resolver
    pub fn new(ast: &'a StoredDefinition, workspace: &'a WorkspaceState) -> Self {
        Self { ast, workspace }
    }

    /// Get the `within` prefix for this file, if any
    pub fn within_prefix(&self) -> Option<String> {
        self.ast.within.as_ref().map(|w| w.to_string())
    }

    /// Resolve a name at the given 0-indexed position.
    ///
    /// Resolution order:
    /// 1. Local components in the containing class
    /// 2. Inherited components (following extends chains, including cross-file)
    /// 3. Import aliases
    /// 4. Nested classes in the containing class
    /// 5. Top-level classes in this file
    /// 6. Classes relative to `within` prefix
    /// 7. Fully qualified workspace symbols
    pub fn resolve(&self, name: &str, line: u32, col: u32) -> Option<ResolvedSymbol<'a>> {
        // Convert to 1-indexed for internal use
        let line = line + 1;
        let col = col + 1;

        // Find the containing class
        let containing_class = self.class_at(line, col);

        if let Some(class) = containing_class {
            // 1. Check direct components
            if let Some(component) = class.components.get(name) {
                return Some(ResolvedSymbol::LocalComponent {
                    component,
                    defined_in: class,
                    inherited_via: None,
                });
            }

            // 2. Check inherited components (including cross-file)
            if let Some((component, defined_in, inherited_via)) =
                self.find_inherited_component(class, name)
            {
                return Some(ResolvedSymbol::LocalComponent {
                    component,
                    defined_in,
                    inherited_via: Some(inherited_via),
                });
            }

            // 3. Check import aliases
            if let Some(resolved_path) = self.resolve_import_alias(class, name) {
                // Look up the resolved path in workspace
                if let Some(sym) = self.workspace.lookup_symbol(&resolved_path) {
                    return Some(ResolvedSymbol::WorkspaceSymbol(sym));
                }
            }

            // 4. Check nested classes
            if let Some(nested) = class.classes.get(name) {
                return Some(ResolvedSymbol::LocalClass(nested));
            }
        }

        // 5. Check top-level classes in this file
        if let Some(class) = self.ast.class_list.get(name) {
            return Some(ResolvedSymbol::LocalClass(class));
        }

        // 6. Try with `within` prefix
        if let Some(within) = self.within_prefix() {
            let qualified = format!("{}.{}", within, name);
            if let Some(sym) = self.workspace.lookup_symbol(&qualified) {
                return Some(ResolvedSymbol::WorkspaceSymbol(sym));
            }
        }

        // 7. Try direct workspace lookup
        if let Some(sym) = self.workspace.lookup_symbol(name) {
            return Some(ResolvedSymbol::WorkspaceSymbol(sym));
        }

        None
    }

    /// Resolve a qualified name (like "Interfaces.DiscreteSISO" or "SI.Mass")
    ///
    /// Resolution order:
    /// 1. Check if first part is an import alias
    /// 2. Try relative to containing class
    /// 3. Try relative to `within` prefix
    /// 4. Try as fully qualified name
    pub fn resolve_qualified(
        &self,
        qualified_name: &str,
        line: u32,
        col: u32,
    ) -> Option<ResolvedSymbol<'a>> {
        let parts: Vec<&str> = qualified_name.split('.').collect();
        if parts.is_empty() {
            return None;
        }

        // Convert to 1-indexed
        let line = line + 1;
        let col = col + 1;

        let first_part = parts[0];
        let rest_parts = &parts[1..];

        // Find the containing class for import resolution
        if let Some(class) = self.class_at(line, col) {
            // 1. Check if first part is an import alias
            if let Some(resolved_path) = self.resolve_import_alias(class, first_part) {
                let full_qualified = if rest_parts.is_empty() {
                    resolved_path
                } else {
                    format!("{}.{}", resolved_path, rest_parts.join("."))
                };

                if let Some(sym) = self.workspace.lookup_symbol(&full_qualified) {
                    return Some(ResolvedSymbol::WorkspaceSymbol(sym));
                }
            }

            // 2. Try relative to containing class's qualified name
            let class_qualified = self.get_qualified_class_name(&class.name.text);
            let relative_to_class = format!("{}.{}", class_qualified, qualified_name);
            if let Some(sym) = self.workspace.lookup_symbol(&relative_to_class) {
                return Some(ResolvedSymbol::WorkspaceSymbol(sym));
            }
        }

        // 3. Try relative to `within` prefix
        if let Some(within) = self.within_prefix() {
            let relative_to_within = format!("{}.{}", within, qualified_name);
            if let Some(sym) = self.workspace.lookup_symbol(&relative_to_within) {
                return Some(ResolvedSymbol::WorkspaceSymbol(sym));
            }
        }

        // 4. Try as fully qualified name
        if let Some(sym) = self.workspace.lookup_symbol(qualified_name) {
            return Some(ResolvedSymbol::WorkspaceSymbol(sym));
        }

        // 5. Also check local classes for qualified names like "OuterClass.InnerClass"
        if parts.len() >= 2 {
            if let Some(outer) = self.ast.class_list.get(first_part) {
                let mut current = outer;
                for part in rest_parts {
                    if let Some(nested) = current.classes.get(*part) {
                        current = nested;
                    } else {
                        return None;
                    }
                }
                return Some(ResolvedSymbol::LocalClass(current));
            }
        }

        None
    }

    /// Get the fully qualified name for a class, considering `within` clause
    fn get_qualified_class_name(&self, class_name: &str) -> String {
        if let Some(within) = self.within_prefix() {
            format!("{}.{}", within, class_name)
        } else {
            class_name.to_string()
        }
    }

    /// Resolve an import alias to its full path
    fn resolve_import_alias(&self, class: &ClassDefinition, alias: &str) -> Option<String> {
        for import in &class.imports {
            match import {
                Import::Renamed {
                    alias: alias_token,
                    path,
                    ..
                } => {
                    if alias_token.text == alias {
                        return Some(path.to_string());
                    }
                }
                Import::Qualified { path, .. } => {
                    // For `import A.B.C;`, the alias is "C"
                    if let Some(last) = path.name.last() {
                        if last.text == alias {
                            return Some(path.to_string());
                        }
                    }
                }
                _ => {}
            }
        }

        // Recursively check parent/outer classes (for imports at enclosing scope)
        // Note: This is a simplification; proper Modelica lookup is more complex

        None
    }

    /// Find a component inherited through extends clauses, including cross-file.
    fn find_inherited_component(
        &self,
        class: &'a ClassDefinition,
        name: &str,
    ) -> Option<(&'a Component, &'a ClassDefinition, String)> {
        for ext in &class.extends {
            let base_name = ext.comp.to_string();

            // Try to find the base class locally first
            if let Some(base_class) = self.find_class_locally(&base_name) {
                // Check direct components in base class
                if let Some(component) = base_class.components.get(name) {
                    return Some((component, base_class, base_name));
                }

                // Recursively check base class's extends
                if let Some(result) = self.find_inherited_component(base_class, name) {
                    return Some(result);
                }
            } else {
                // Try workspace lookup for cross-file inheritance
                let qualified_base = self.resolve_class_name(&base_name);
                if let Some(_sym) = self.workspace.lookup_symbol(&qualified_base) {
                    // We found the base class in the workspace, but we can't return
                    // the component directly without parsing that file.
                    // For now, we'll note this as a limitation.
                    // A full implementation would load and parse the base class file.

                    // Try to get the parsed AST from the workspace
                    if let Some(base_ast) = self.workspace.get_parsed_ast_by_name(&qualified_base) {
                        // Find the class in the AST
                        if let Some(base_class) = self.find_class_in_ast(base_ast, &base_name) {
                            if let Some(component) = base_class.components.get(name) {
                                return Some((component, base_class, base_name));
                            }
                            // Recursively check - but we need a new resolver for this AST
                            // This is getting complex; for now, check one level
                        }
                    }
                }
            }
        }
        None
    }

    /// Find a class in a parsed AST by simple name
    fn find_class_in_ast<'b>(
        &self,
        ast: &'b StoredDefinition,
        name: &str,
    ) -> Option<&'b ClassDefinition> {
        // Check if name is qualified
        let parts: Vec<&str> = name.split('.').collect();

        if parts.len() == 1 {
            // Simple name - check top-level
            return ast.class_list.get(name);
        }

        // For qualified names, we need to consider the within clause
        // The class name in the AST is just the simple name
        let simple_name = parts.last()?;
        ast.class_list.get(*simple_name)
    }

    /// Resolve a class name, trying with `within` prefix if needed
    fn resolve_class_name(&self, name: &str) -> String {
        // If already qualified, return as-is
        if name.contains('.') {
            // Still try with within prefix first
            if let Some(within) = self.within_prefix() {
                let qualified = format!("{}.{}", within, name);
                if self.workspace.lookup_symbol(&qualified).is_some() {
                    return qualified;
                }
            }
            return name.to_string();
        }

        // Try with within prefix
        if let Some(within) = self.within_prefix() {
            let qualified = format!("{}.{}", within, name);
            if self.workspace.lookup_symbol(&qualified).is_some() {
                return qualified;
            }
        }

        name.to_string()
    }

    /// Find a class locally in this file
    fn find_class_locally(&self, name: &str) -> Option<&'a ClassDefinition> {
        let parts: Vec<&str> = name.split('.').collect();

        if parts.len() == 1 {
            // Simple name - check top-level classes
            if let Some(class) = self.ast.class_list.get(name) {
                return Some(class);
            }
            // Check nested classes in all top-level classes
            for class in self.ast.class_list.values() {
                if let Some(nested) = class.classes.get(name) {
                    return Some(nested);
                }
            }
        } else {
            // Qualified name - navigate through hierarchy
            let first = parts[0];
            if let Some(mut current) = self.ast.class_list.get(first) {
                for part in &parts[1..] {
                    if let Some(nested) = current.classes.get(*part) {
                        current = nested;
                    } else {
                        return None;
                    }
                }
                return Some(current);
            }
        }

        None
    }

    /// Find the innermost class containing the given 1-indexed position
    fn class_at(&self, line: u32, col: u32) -> Option<&'a ClassDefinition> {
        let mut best_match: Option<&ClassDefinition> = None;
        let mut best_start_line = 0u32;

        for class in self.ast.class_list.values() {
            if let Some(found) = Self::find_innermost_class(class, line, col, &mut best_start_line)
            {
                best_match = Some(found);
            }
        }

        best_match
    }

    /// Recursively find the innermost class containing the position
    fn find_innermost_class(
        class: &'a ClassDefinition,
        line: u32,
        col: u32,
        best_start_line: &mut u32,
    ) -> Option<&'a ClassDefinition> {
        let mut result = None;

        if Self::position_in_location(&class.location, line, col)
            && class.location.start_line > *best_start_line
        {
            *best_start_line = class.location.start_line;
            result = Some(class);
        }

        // Check nested classes
        for nested in class.classes.values() {
            if let Some(found) = Self::find_innermost_class(nested, line, col, best_start_line) {
                result = Some(found);
            }
        }

        result
    }

    /// Check if a position is within a location span (1-indexed)
    fn position_in_location(loc: &crate::ir::ast::Location, line: u32, col: u32) -> bool {
        if line < loc.start_line || line > loc.end_line {
            return false;
        }
        if line == loc.start_line && col < loc.start_column {
            return false;
        }
        if line == loc.end_line && col > loc.end_column {
            return false;
        }
        true
    }
}

#[cfg(test)]
mod tests {
    // Tests would go here
}
