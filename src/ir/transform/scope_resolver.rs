//! Scope resolver for Modelica AST.
//!
//! Provides utilities for determining scope context at a given source position,
//! resolving names, and finding visible symbols (components, classes, etc.).
//!
//! This module is used by the LSP for hover, completion, go-to-definition, etc.,
//! and can also be used by the compiler for better error messages.
//!
//! ## Design
//!
//! The `ScopeResolver` provides single-file scope resolution. For multi-file
//! workspace resolution, use it with an optional `SymbolLookup` implementation
//! that provides cross-file symbol lookup.
//!
//! ## Canonical Functions
//!
//! This module provides the canonical implementations for class/symbol lookup
//! that should be reused across the codebase (compiler, LSP, etc.):
//!
//! - [`find_class_in_ast`]: Find a class by qualified name in an AST
//! - [`resolve_type_candidates`]: Generate possible qualified names walking up package hierarchy
//! - [`ImportResolver`]: Resolve import aliases to fully qualified paths

use std::collections::HashMap;

use crate::ir::ast::{ClassDefinition, Component, Import, Location, StoredDefinition};

/// Information about a symbol from workspace lookup.
///
/// This is a simplified view of a symbol that doesn't require owning the AST.
/// Used by `SymbolLookup` trait to provide cross-file symbol information.
///
/// Note: This is distinct from `SymbolInfo` trait in `ir::analysis::symbol_trait`
/// which is for type analysis. This struct is specifically for workspace/cross-file
/// symbol discovery.
#[derive(Debug, Clone)]
pub struct ExternalSymbol {
    /// Fully qualified name (e.g., "MyPackage.SubPackage.MyModel")
    pub qualified_name: String,
    /// File path or URI string
    pub location: String,
    /// Line number (0-based)
    pub line: u32,
    /// Column number (0-based)
    pub column: u32,
    /// Symbol category
    pub kind: SymbolCategory,
    /// Brief description
    pub detail: Option<String>,
}

/// Category of a symbol
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolCategory {
    Package,
    Model,
    Class,
    Block,
    Connector,
    Record,
    Type,
    Function,
    Operator,
    Component,
    Parameter,
    Constant,
}

/// Trait for cross-file symbol lookup.
///
/// Implement this trait to enable multi-file resolution in `ScopeResolver`.
/// The LSP's `WorkspaceState` implements this trait.
pub trait SymbolLookup {
    /// Look up a symbol by its qualified name.
    fn lookup_symbol(&self, name: &str) -> Option<ExternalSymbol>;

    /// Get the parsed AST for a symbol's containing file.
    ///
    /// This allows the resolver to navigate into cross-file base classes.
    fn get_ast_for_symbol(&self, qualified_name: &str) -> Option<&StoredDefinition>;
}

/// A resolved symbol with its origin information
#[derive(Debug, Clone)]
pub enum ResolvedSymbol<'a> {
    /// A component (variable, parameter, etc.) with optional inheritance info
    Component {
        component: &'a Component,
        /// The class where this component is defined
        defined_in: &'a ClassDefinition,
        /// If inherited, the name of the base class it came from
        inherited_via: Option<String>,
    },
    /// A class definition
    Class(&'a ClassDefinition),
    /// A symbol resolved from cross-file workspace lookup
    External(ExternalSymbol),
}

/// Scope resolver for querying the AST at specific positions.
///
/// Supports both single-file and multi-file resolution:
/// - Without a `SymbolLookup`, resolves symbols within the current file only
/// - With a `SymbolLookup`, also resolves cross-file symbols via workspace lookup
pub struct ScopeResolver<'a, L: SymbolLookup + ?Sized = dyn SymbolLookup> {
    ast: &'a StoredDefinition,
    /// Optional workspace lookup for cross-file resolution
    lookup: Option<&'a L>,
}

impl<'a> ScopeResolver<'a, dyn SymbolLookup> {
    /// Create a new scope resolver for single-file resolution
    pub fn new(ast: &'a StoredDefinition) -> Self {
        Self { ast, lookup: None }
    }
}

impl<'a, L: SymbolLookup + ?Sized> ScopeResolver<'a, L> {
    /// Create a new scope resolver with workspace lookup for multi-file resolution
    pub fn with_lookup(ast: &'a StoredDefinition, lookup: &'a L) -> Self {
        Self {
            ast,
            lookup: Some(lookup),
        }
    }

    /// Get the `within` prefix for this file, if any
    pub fn within_prefix(&self) -> Option<String> {
        self.ast.within.as_ref().map(|w| w.to_string())
    }

    /// Find the innermost class containing the given position.
    ///
    /// Position is 1-indexed (matching source file line/column numbers).
    pub fn class_at(&self, line: u32, col: u32) -> Option<&'a ClassDefinition> {
        let mut best_match: Option<&ClassDefinition> = None;
        let mut best_start_line = 0u32;

        // Check top-level classes
        for class in self.ast.class_list.values() {
            if Self::position_in_location(&class.location, line, col)
                && class.location.start_line > best_start_line
            {
                best_start_line = class.location.start_line;
                best_match = Some(class);
            }

            // Check nested classes (recursively would be better for deep nesting)
            for nested in class.classes.values() {
                if Self::position_in_location(&nested.location, line, col)
                    && nested.location.start_line > best_start_line
                {
                    best_start_line = nested.location.start_line;
                    best_match = Some(nested);
                }
            }
        }

        best_match
    }

    /// Find the innermost class containing the given 0-indexed position.
    ///
    /// This is a convenience method for LSP which uses 0-indexed positions.
    pub fn class_at_0indexed(&self, line: u32, col: u32) -> Option<&'a ClassDefinition> {
        self.class_at(line + 1, col + 1)
    }

    /// Resolve a name at the given position.
    ///
    /// Looks up the name in the scope at the given position, checking:
    /// 1. Direct components in the containing class
    /// 2. Inherited components from extends clauses (including cross-file)
    /// 3. Import aliases
    /// 4. Nested classes
    /// 5. Top-level classes in this file
    /// 6. Classes relative to `within` prefix (if workspace lookup available)
    /// 7. Fully qualified workspace symbols (if workspace lookup available)
    ///
    /// Position is 1-indexed.
    pub fn resolve(&self, name: &str, line: u32, col: u32) -> Option<ResolvedSymbol<'a>> {
        // First, find the containing class
        let containing_class = self.class_at(line, col);

        if let Some(class) = containing_class {
            // 1. Check direct components
            if let Some(component) = class.components.get(name) {
                return Some(ResolvedSymbol::Component {
                    component,
                    defined_in: class,
                    inherited_via: None,
                });
            }

            // 2. Check inherited components (including cross-file)
            if let Some((component, base_class, base_name)) =
                self.find_inherited_component(class, name)
            {
                return Some(ResolvedSymbol::Component {
                    component,
                    defined_in: base_class,
                    inherited_via: Some(base_name),
                });
            }

            // 3. Check import aliases
            if let Some(resolved_path) = self.resolve_import_alias(class, name)
                && let Some(lookup) = &self.lookup
                && let Some(sym) = lookup.lookup_symbol(&resolved_path)
            {
                return Some(ResolvedSymbol::External(sym));
            }

            // 4. Check nested classes
            if let Some(nested) = class.classes.get(name) {
                return Some(ResolvedSymbol::Class(nested));
            }
        }

        // 5. Check top-level classes in this file
        if let Some(class) = self.ast.class_list.get(name) {
            return Some(ResolvedSymbol::Class(class));
        }

        // 6. Try with `within` prefix (if workspace lookup available)
        if let Some(lookup) = &self.lookup {
            if let Some(within) = self.within_prefix() {
                let qualified = format!("{}.{}", within, name);
                if let Some(sym) = lookup.lookup_symbol(&qualified) {
                    return Some(ResolvedSymbol::External(sym));
                }
            }

            // 7. Try direct workspace lookup
            if let Some(sym) = lookup.lookup_symbol(name) {
                return Some(ResolvedSymbol::External(sym));
            }
        }

        None
    }

    /// Resolve a name at the given 0-indexed position.
    pub fn resolve_0indexed(&self, name: &str, line: u32, col: u32) -> Option<ResolvedSymbol<'a>> {
        self.resolve(name, line + 1, col + 1)
    }

    /// Get all components visible at the given position (direct + inherited).
    ///
    /// Position is 1-indexed.
    pub fn visible_components(&self, line: u32, col: u32) -> Vec<ResolvedSymbol<'a>> {
        let mut result = Vec::new();

        if let Some(containing_class) = self.class_at(line, col) {
            // Add direct components
            for component in containing_class.components.values() {
                result.push(ResolvedSymbol::Component {
                    component,
                    defined_in: containing_class,
                    inherited_via: None,
                });
            }

            // Add inherited components
            for ext in &containing_class.extends {
                let base_name = ext.comp.to_string();
                if let Some(base_class) = self.ast.class_list.get(&base_name) {
                    for component in base_class.components.values() {
                        // Don't add if already present (overridden)
                        if !containing_class.components.contains_key(&component.name) {
                            result.push(ResolvedSymbol::Component {
                                component,
                                defined_in: base_class,
                                inherited_via: Some(base_name.clone()),
                            });
                        }
                    }
                }
            }
        }

        result
    }

    /// Resolve a qualified name (like "Interfaces.DiscreteSISO" or "SI.Mass").
    ///
    /// Resolution order:
    /// 1. Check if first part is an import alias
    /// 2. Try relative to containing class
    /// 3. Try relative to `within` prefix
    /// 4. Try as fully qualified name
    /// 5. Try as local nested class path
    ///
    /// Position is 1-indexed.
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

                if let Some(lookup) = &self.lookup
                    && let Some(sym) = lookup.lookup_symbol(&full_qualified)
                {
                    return Some(ResolvedSymbol::External(sym));
                }
            }

            // 2. Try relative to containing class's qualified name
            if let Some(lookup) = &self.lookup {
                let class_qualified = self.get_qualified_class_name(&class.name.text);
                let relative_to_class = format!("{}.{}", class_qualified, qualified_name);
                if let Some(sym) = lookup.lookup_symbol(&relative_to_class) {
                    return Some(ResolvedSymbol::External(sym));
                }
            }
        }

        // 3. Try relative to `within` prefix
        if let Some(lookup) = &self.lookup {
            if let Some(within) = self.within_prefix() {
                let relative_to_within = format!("{}.{}", within, qualified_name);
                if let Some(sym) = lookup.lookup_symbol(&relative_to_within) {
                    return Some(ResolvedSymbol::External(sym));
                }
            }

            // 4. Try as fully qualified name
            if let Some(sym) = lookup.lookup_symbol(qualified_name) {
                return Some(ResolvedSymbol::External(sym));
            }
        }

        // 5. Check local nested class path (e.g., "OuterClass.InnerClass")
        if parts.len() >= 2
            && let Some(outer) = self.ast.class_list.get(first_part)
        {
            let mut current = outer;
            for part in rest_parts {
                if let Some(nested) = current.classes.get(*part) {
                    current = nested;
                } else {
                    return None;
                }
            }
            return Some(ResolvedSymbol::Class(current));
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

    /// Resolve an import alias to its full path.
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
                    if let Some(last) = path.name.last()
                        && last.text == alias
                    {
                        return Some(path.to_string());
                    }
                }
                _ => {}
            }
        }
        None
    }

    /// Find a class locally in this file.
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

    /// Resolve a class name, trying with `within` prefix if needed.
    fn resolve_class_name(&self, name: &str) -> String {
        // If already qualified and we have lookup, check if it exists
        if name.contains('.') {
            if let Some(lookup) = &self.lookup {
                // Try with within prefix first
                if let Some(within) = self.within_prefix() {
                    let qualified = format!("{}.{}", within, name);
                    if lookup.lookup_symbol(&qualified).is_some() {
                        return qualified;
                    }
                }
            }
            return name.to_string();
        }

        // Try with within prefix
        if let Some(lookup) = &self.lookup
            && let Some(within) = self.within_prefix()
        {
            let qualified = format!("{}.{}", within, name);
            if lookup.lookup_symbol(&qualified).is_some() {
                return qualified;
            }
        }

        name.to_string()
    }

    /// Find a component inherited through extends clauses.
    ///
    /// Returns the component, the class it's defined in, and the base class name.
    /// Supports cross-file inheritance when a `SymbolLookup` is available.
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
            } else if let Some(lookup) = &self.lookup {
                // Try workspace lookup for cross-file inheritance
                let qualified_base = self.resolve_class_name(&base_name);
                if let Some(base_ast) = lookup.get_ast_for_symbol(&qualified_base) {
                    // Find the class in the external AST
                    if let Some(base_class) = Self::find_class_in_ast(base_ast, &base_name)
                        && let Some(component) = base_class.components.get(name)
                    {
                        return Some((component, base_class, base_name));
                    }
                    // Note: recursive cross-file lookup would require more complex handling
                }
            }
        }
        None
    }

    /// Find a class in a parsed AST by simple or qualified name.
    fn find_class_in_ast<'b>(ast: &'b StoredDefinition, name: &str) -> Option<&'b ClassDefinition> {
        let parts: Vec<&str> = name.split('.').collect();

        if parts.len() == 1 {
            // Simple name - check top-level
            return ast.class_list.get(name);
        }

        // For qualified names, the class name in the AST is just the simple name
        let simple_name = parts.last()?;
        ast.class_list.get(*simple_name)
    }

    /// Check if a position (line, col) is within a location span.
    ///
    /// Both position and location use 1-indexed line/column numbers.
    fn position_in_location(loc: &Location, line: u32, col: u32) -> bool {
        // Check if position is within the location's start and end lines
        if line < loc.start_line || line > loc.end_line {
            return false;
        }
        // If on the start line, check column is at or after start
        if line == loc.start_line && col < loc.start_column {
            return false;
        }
        // If on the end line, check column is at or before end
        if line == loc.end_line && col > loc.end_column {
            return false;
        }
        true
    }
}

// =============================================================================
// Canonical standalone functions for class/symbol lookup
// =============================================================================

/// Find a class definition in an AST by its qualified name.
///
/// This is the canonical implementation for finding classes. It handles:
/// - Simple names (e.g., "MyClass")
/// - Qualified names (e.g., "Package.SubPackage.MyClass")
/// - Files with `within` clauses (e.g., "within Modelica.Blocks;")
/// - Nested class hierarchies
///
/// # Arguments
/// * `ast` - The parsed AST (StoredDefinition)
/// * `qualified_name` - The fully qualified or simple name to find
///
/// # Returns
/// The class definition if found, or None.
///
/// # Example
///
/// To find "Modelica.Blocks.Continuous.PID" in an AST with `within Modelica.Blocks.Continuous`:
/// ```text
/// let class = find_class_in_ast(&ast, "Modelica.Blocks.Continuous.PID");
/// ```
pub fn find_class_in_ast<'a>(
    ast: &'a StoredDefinition,
    qualified_name: &str,
) -> Option<&'a ClassDefinition> {
    let parts: Vec<&str> = qualified_name.split('.').collect();
    if parts.is_empty() {
        return None;
    }

    // Strategy 1: Try simple name lookup (handles files where class is stored by simple name)
    let simple_name = parts.last().unwrap();
    if let Some(class) = ast.class_list.get(*simple_name) {
        return Some(class);
    }

    // Strategy 2: Handle `within` clause
    // If the AST has "within X.Y", and we're looking for "X.Y.Z.W", strip the prefix
    if let Some(within) = &ast.within {
        let within_str = within.to_string();
        let within_prefix = format!("{}.", within_str);
        if qualified_name.starts_with(&within_prefix) {
            let remainder = &qualified_name[within_prefix.len()..];
            let remainder_parts: Vec<&str> = remainder.split('.').collect();
            if !remainder_parts.is_empty()
                && let Some(class) = ast.class_list.get(remainder_parts[0])
            {
                if remainder_parts.len() == 1 {
                    return Some(class);
                }
                return find_nested_class(class, &remainder_parts[1..]);
            }
        }
    }

    // Strategy 3: Navigate from top-level class (for nested classes in same file)
    let first_part = parts[0];
    if let Some(class) = ast.class_list.get(first_part) {
        if parts.len() == 1 {
            return Some(class);
        }
        return find_nested_class(class, &parts[1..]);
    }

    // Strategy 4: Try the full qualified name as a direct key (rare but possible)
    ast.class_list.get(qualified_name)
}

/// Navigate to a nested class by path components.
///
/// # Arguments
/// * `parent` - The parent class to start from
/// * `path` - Remaining path components (e.g., ["SubClass", "InnerClass"])
pub fn find_nested_class<'a>(
    parent: &'a ClassDefinition,
    path: &[&str],
) -> Option<&'a ClassDefinition> {
    if path.is_empty() {
        return Some(parent);
    }

    if let Some(child) = parent.classes.get(path[0]) {
        if path.len() == 1 {
            return Some(child);
        }
        return find_nested_class(child, &path[1..]);
    }

    None
}

/// Generate all possible qualified name candidates for a type name.
///
/// When resolving a relative type name like "Interfaces.SISO" from within
/// "Modelica.Blocks.Continuous.PID", this generates candidates by walking up
/// the package hierarchy:
///
/// 1. Modelica.Blocks.Continuous.Interfaces.SISO
/// 2. Modelica.Blocks.Interfaces.SISO
/// 3. Modelica.Interfaces.SISO
/// 4. Interfaces.SISO (as-is)
///
/// # Arguments
/// * `current_qualified` - The fully qualified name of the current context (e.g., "Modelica.Blocks.Continuous.PID")
/// * `type_name` - The type name to resolve (e.g., "Interfaces.SISO" or "SISO")
///
/// # Returns
/// Vector of candidate names in order of preference (most specific first).
pub fn resolve_type_candidates(current_qualified: &str, type_name: &str) -> Vec<String> {
    let mut candidates = Vec::new();

    // Get the package path (everything except the class name)
    let current_parts: Vec<&str> = current_qualified.split('.').collect();
    if current_parts.len() > 1 {
        // Start from the immediate parent package and work up
        for i in (1..current_parts.len()).rev() {
            let prefix = current_parts[..i].join(".");
            candidates.push(format!("{}.{}", prefix, type_name));
        }
    }

    // Always try the type_name as-is (might be fully qualified or top-level)
    candidates.push(type_name.to_string());

    candidates
}

/// Helper for resolving import aliases to fully qualified paths.
///
/// Builds a mapping from alias names to fully qualified paths based on
/// the import declarations in a class.
///
/// # Example
///
/// Given the following imports:
/// ```text
/// import Modelica.Blocks.Continuous.PID;
/// import SI = Modelica.Units.SI;
/// ```
///
/// The resolver maps aliases to their fully qualified paths:
/// - `"PID"` → `"Modelica.Blocks.Continuous.PID"`
/// - `"SI"` → `"Modelica.Units.SI"`
#[derive(Debug, Default)]
pub struct ImportResolver {
    /// Maps alias name -> fully qualified path
    aliases: HashMap<String, String>,
}

impl ImportResolver {
    /// Create a new empty import resolver.
    pub fn new() -> Self {
        Self {
            aliases: HashMap::new(),
        }
    }

    /// Build an import resolver from a list of imports.
    pub fn from_imports(imports: &[Import]) -> Self {
        let mut resolver = Self::new();
        for import in imports {
            match import {
                Import::Renamed { alias, path, .. } => {
                    resolver
                        .aliases
                        .insert(alias.text.clone(), path.to_string());
                }
                Import::Qualified { path, .. } => {
                    // For `import A.B.C;`, the alias is "C"
                    if let Some(last) = path.name.last() {
                        resolver.aliases.insert(last.text.clone(), path.to_string());
                    }
                }
                Import::Selective { path, names, .. } => {
                    // For `import A.B.{C, D};`, create entries for each name
                    let base_path = path.to_string();
                    for name in names {
                        resolver
                            .aliases
                            .insert(name.text.clone(), format!("{}.{}", base_path, name.text));
                    }
                }
                Import::Unqualified { .. } => {
                    // Wildcard imports need runtime lookup, can't pre-resolve
                }
            }
        }
        resolver
    }

    /// Resolve an alias to its fully qualified path.
    pub fn resolve(&self, alias: &str) -> Option<&str> {
        self.aliases.get(alias).map(|s| s.as_str())
    }

    /// Get all aliases as an iterator.
    pub fn iter(&self) -> impl Iterator<Item = (&str, &str)> {
        self.aliases.iter().map(|(k, v)| (k.as_str(), v.as_str()))
    }
}

/// Collect all inherited components from a class and its base classes.
///
/// This recursively traverses the inheritance chain and collects all components
/// that are visible in the derived class, with proper handling of:
/// - Direct components in the base class
/// - Components inherited from grandparent classes
/// - Override semantics (derived class components take precedence)
///
/// # Arguments
/// * `class` - The class to collect inherited components from
/// * `peer_classes` - Map of peer classes in the same file (for resolving extends)
///
/// # Returns
/// HashMap mapping component name to (Component reference, base class name)
pub fn collect_inherited_components<'a>(
    class: &'a ClassDefinition,
    peer_classes: &'a indexmap::IndexMap<String, ClassDefinition>,
) -> HashMap<String, (&'a Component, String)> {
    let mut result = HashMap::new();
    collect_inherited_recursive(class, peer_classes, &mut result);
    result
}

fn collect_inherited_recursive<'a>(
    class: &'a ClassDefinition,
    peer_classes: &'a indexmap::IndexMap<String, ClassDefinition>,
    result: &mut HashMap<String, (&'a Component, String)>,
) {
    for ext in &class.extends {
        let base_name = ext.comp.to_string();
        if let Some(base_class) = peer_classes.get(&base_name) {
            // Add components from base class (don't override existing)
            for (comp_name, comp) in &base_class.components {
                if !result.contains_key(comp_name) {
                    result.insert(comp_name.clone(), (comp, base_name.clone()));
                }
            }
            // Recursively collect from grandparent classes
            collect_inherited_recursive(base_class, peer_classes, result);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::modelica_grammar::ModelicaGrammar;
    use crate::modelica_parser::parse;

    fn parse_test_code(code: &str) -> StoredDefinition {
        let mut grammar = ModelicaGrammar::new();
        parse(code, "test.mo", &mut grammar).expect("Failed to parse test code");
        grammar.modelica.expect("No AST produced")
    }

    #[test]
    fn test_class_at_position() {
        let code = r#"
class Outer
  Real x;
  class Inner
    Real y;
  end Inner;
end Outer;
"#;
        let ast = parse_test_code(code);
        let resolver = ScopeResolver::new(&ast);

        // Line 3 should be in Outer (Real x;)
        let class = resolver.class_at(3, 5);
        assert!(class.is_some());
        assert_eq!(class.unwrap().name.text, "Outer");

        // Line 5 should be in Inner (Real y;)
        let class = resolver.class_at(5, 5);
        assert!(class.is_some());
        assert_eq!(class.unwrap().name.text, "Inner");
    }

    #[test]
    fn test_resolve_direct_component() {
        let code = r#"
class Test
  Real x;
  Real y;
equation
  x = y;
end Test;
"#;
        let ast = parse_test_code(code);
        let resolver = ScopeResolver::new(&ast);

        // Resolve 'x' at line 6 (in equation section)
        let symbol = resolver.resolve("x", 6, 3);
        assert!(symbol.is_some());
        if let Some(ResolvedSymbol::Component {
            component,
            inherited_via,
            ..
        }) = symbol
        {
            assert_eq!(component.name, "x");
            assert!(inherited_via.is_none());
        } else {
            panic!("Expected Component");
        }
    }

    #[test]
    fn test_resolve_inherited_component() {
        let code = r#"
class Base
  Real v;
end Base;

class Derived
  extends Base;
equation
  v = 1;
end Derived;
"#;
        let ast = parse_test_code(code);
        let resolver = ScopeResolver::new(&ast);

        // Resolve 'v' at line 9 (in Derived's equation section)
        let symbol = resolver.resolve("v", 9, 3);
        assert!(symbol.is_some());
        if let Some(ResolvedSymbol::Component {
            component,
            defined_in,
            inherited_via,
        }) = symbol
        {
            assert_eq!(component.name, "v");
            assert_eq!(defined_in.name.text, "Base");
            assert!(inherited_via.is_some());
        } else {
            panic!("Expected Component");
        }
    }

    #[test]
    fn test_resolve_class() {
        let code = r#"
class MyClass
  Real x;
end MyClass;
"#;
        let ast = parse_test_code(code);
        let resolver = ScopeResolver::new(&ast);

        // Resolve 'MyClass' from anywhere
        let symbol = resolver.resolve("MyClass", 1, 1);
        assert!(symbol.is_some());
        if let Some(ResolvedSymbol::Class(class)) = symbol {
            assert_eq!(class.name.text, "MyClass");
        } else {
            panic!("Expected Class");
        }
    }

    // =========================================================================
    // Tests for canonical standalone functions
    // =========================================================================

    #[test]
    fn test_find_class_in_ast_simple() {
        let code = r#"
model TestModel
    Real x;
end TestModel;
"#;
        let ast = parse_test_code(code);
        let result = find_class_in_ast(&ast, "TestModel");
        assert!(result.is_some());
        assert!(result.unwrap().components.contains_key("x"));
    }

    #[test]
    fn test_find_class_in_ast_with_within() {
        let code = r#"
within Modelica.Blocks.Continuous;
model PID
    Real x;
end PID;
"#;
        let ast = parse_test_code(code);

        // Should find by qualified name
        let result = find_class_in_ast(&ast, "Modelica.Blocks.Continuous.PID");
        assert!(result.is_some(), "Should find PID by qualified name");
        assert!(result.unwrap().components.contains_key("x"));

        // Should also find by simple name
        let result = find_class_in_ast(&ast, "PID");
        assert!(result.is_some(), "Should find PID by simple name");
    }

    #[test]
    fn test_find_class_in_ast_nested() {
        let code = r#"
package MyPackage
    model InnerModel
        Real z;
    end InnerModel;
end MyPackage;
"#;
        let ast = parse_test_code(code);

        // Find nested class
        let result = find_class_in_ast(&ast, "MyPackage.InnerModel");
        assert!(result.is_some(), "Should find nested InnerModel");
        assert!(result.unwrap().components.contains_key("z"));
    }

    #[test]
    fn test_resolve_type_candidates() {
        // Test with deeply nested context
        let candidates =
            resolve_type_candidates("Modelica.Blocks.Continuous.PID", "Interfaces.SISO");
        assert_eq!(candidates.len(), 4);
        assert_eq!(candidates[0], "Modelica.Blocks.Continuous.Interfaces.SISO");
        assert_eq!(candidates[1], "Modelica.Blocks.Interfaces.SISO");
        assert_eq!(candidates[2], "Modelica.Interfaces.SISO");
        assert_eq!(candidates[3], "Interfaces.SISO");

        // Test with simple context
        let candidates = resolve_type_candidates("PID", "SISO");
        assert_eq!(candidates.len(), 1);
        assert_eq!(candidates[0], "SISO");

        // Test with two-level context
        let candidates = resolve_type_candidates("Modelica.PID", "SISO");
        assert_eq!(candidates.len(), 2);
        assert_eq!(candidates[0], "Modelica.SISO");
        assert_eq!(candidates[1], "SISO");
    }

    #[test]
    fn test_import_resolver() {
        let code = r#"
model Test
    import Modelica.Blocks.Continuous.PID;
    import SI = Modelica.Units.SI;
    import Modelica.Constants.{pi, e};
end Test;
"#;
        let ast = parse_test_code(code);
        let class = ast.class_list.get("Test").expect("Test class not found");
        let resolver = ImportResolver::from_imports(&class.imports);

        // Qualified import: PID -> Modelica.Blocks.Continuous.PID
        assert_eq!(
            resolver.resolve("PID"),
            Some("Modelica.Blocks.Continuous.PID")
        );

        // Renamed import: SI -> Modelica.Units.SI
        assert_eq!(resolver.resolve("SI"), Some("Modelica.Units.SI"));

        // Selective import: pi -> Modelica.Constants.pi
        assert_eq!(resolver.resolve("pi"), Some("Modelica.Constants.pi"));
        assert_eq!(resolver.resolve("e"), Some("Modelica.Constants.e"));
    }

    #[test]
    fn test_collect_inherited_components() {
        let code = r#"
class Base
    Real x;
    Real y;
end Base;

class Derived
    extends Base;
    Real z;
end Derived;
"#;
        let ast = parse_test_code(code);
        let derived = ast.class_list.get("Derived").expect("Derived not found");

        let inherited = collect_inherited_components(derived, &ast.class_list);

        // Should have x and y from Base
        assert!(inherited.contains_key("x"), "Should inherit x");
        assert!(inherited.contains_key("y"), "Should inherit y");

        // z is direct, not inherited, so it won't be in the inherited map
        assert!(!inherited.contains_key("z"), "z is direct, not inherited");

        // Check base class name
        let (_, base_name) = inherited.get("x").unwrap();
        assert_eq!(base_name, "Base");
    }
}
