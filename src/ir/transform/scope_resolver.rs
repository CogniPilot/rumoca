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

use crate::ir::ast::{ClassDefinition, Component, Import, Location, StoredDefinition};

/// Information about a symbol from workspace lookup.
///
/// This is a simplified view of a symbol that doesn't require owning the AST.
/// Used by `SymbolLookup` trait to provide cross-file symbol information.
#[derive(Debug, Clone)]
pub struct SymbolInfo {
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
    fn lookup_symbol(&self, name: &str) -> Option<SymbolInfo>;

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
    External(SymbolInfo),
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
            if let Some(resolved_path) = self.resolve_import_alias(class, name) {
                if let Some(lookup) = &self.lookup {
                    if let Some(sym) = lookup.lookup_symbol(&resolved_path) {
                        return Some(ResolvedSymbol::External(sym));
                    }
                }
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

                if let Some(lookup) = &self.lookup {
                    if let Some(sym) = lookup.lookup_symbol(&full_qualified) {
                        return Some(ResolvedSymbol::External(sym));
                    }
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
                return Some(ResolvedSymbol::Class(current));
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
                    if let Some(last) = path.name.last() {
                        if last.text == alias {
                            return Some(path.to_string());
                        }
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
        if let Some(lookup) = &self.lookup {
            if let Some(within) = self.within_prefix() {
                let qualified = format!("{}.{}", within, name);
                if lookup.lookup_symbol(&qualified).is_some() {
                    return qualified;
                }
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
                    if let Some(base_class) = Self::find_class_in_ast(base_ast, &base_name) {
                        if let Some(component) = base_class.components.get(name) {
                            return Some((component, base_class, base_name));
                        }
                        // Note: recursive cross-file lookup would require more complex handling
                    }
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
}
