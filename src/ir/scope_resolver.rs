//! Scope resolver for Modelica AST.
//!
//! Provides utilities for determining scope context at a given source position,
//! resolving names, and finding visible symbols (components, classes, etc.).
//!
//! This module is used by the LSP for hover, completion, go-to-definition, etc.,
//! and can also be used by the compiler for better error messages.

use super::ast::{ClassDefinition, Component, Location, StoredDefinition};

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
}

/// Scope resolver for querying the AST at specific positions.
pub struct ScopeResolver<'a> {
    ast: &'a StoredDefinition,
}

impl<'a> ScopeResolver<'a> {
    /// Create a new scope resolver for the given AST
    pub fn new(ast: &'a StoredDefinition) -> Self {
        Self { ast }
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
    /// 2. Inherited components from extends clauses
    /// 3. Nested classes
    /// 4. Top-level classes
    ///
    /// Position is 1-indexed.
    pub fn resolve(&self, name: &str, line: u32, col: u32) -> Option<ResolvedSymbol<'a>> {
        // First, find the containing class
        if let Some(containing_class) = self.class_at(line, col) {
            // Check direct components
            if let Some(component) = containing_class.components.get(name) {
                return Some(ResolvedSymbol::Component {
                    component,
                    defined_in: containing_class,
                    inherited_via: None,
                });
            }

            // Check inherited components
            if let Some((component, base_class, base_name)) =
                self.find_inherited_component(containing_class, name)
            {
                return Some(ResolvedSymbol::Component {
                    component,
                    defined_in: base_class,
                    inherited_via: Some(base_name),
                });
            }

            // Check nested classes
            if let Some(nested) = containing_class.classes.get(name) {
                return Some(ResolvedSymbol::Class(nested));
            }
        }

        // Check top-level classes
        if let Some(class) = self.ast.class_list.get(name) {
            return Some(ResolvedSymbol::Class(class));
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

    /// Find a component inherited through extends clauses.
    ///
    /// Returns the component, the class it's defined in, and the base class name.
    fn find_inherited_component(
        &self,
        class: &'a ClassDefinition,
        name: &str,
    ) -> Option<(&'a Component, &'a ClassDefinition, String)> {
        for ext in &class.extends {
            let base_name = ext.comp.to_string();
            if let Some(base_class) = self.ast.class_list.get(&base_name) {
                // Check direct components in base class
                if let Some(component) = base_class.components.get(name) {
                    return Some((component, base_class, base_name));
                }

                // Recursively check base class's extends
                if let Some(result) = self.find_inherited_component(base_class, name) {
                    return Some(result);
                }
            }
        }
        None
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
