//! Symbol collection and analysis for Modelica code.
//!
//! This module provides unified symbol collection and analysis functionality
//! used by linting, diagnostics, and semantic analysis.
//!
//! ## Visitor-based Symbol Collection
//!
//! The [`SymbolCollectorVisitor`] provides a clean, visitor-based approach to
//! collecting symbol references from the AST. It uses the standard [`Visitor`]
//! trait from [`crate::ir::visitor`].

use std::collections::{HashMap, HashSet};

use crate::ir::ast::{
    Causality, ClassDefinition, ClassType, Component, ComponentReference, Expression, ForIndex,
    Variability,
};
use crate::ir::visitor::{Visitable, Visitor};

/// Information about a defined symbol for analysis.
///
/// This struct captures all relevant information about a declared symbol
/// (variable, parameter, constant, or nested class) for use in semantic analysis.
#[derive(Clone, Debug)]
pub struct DefinedSymbol {
    /// Source line number (1-based)
    pub line: u32,
    /// Source column number (1-based)
    pub col: u32,
    /// Whether this symbol is a parameter
    pub is_parameter: bool,
    /// Whether this symbol is a constant
    pub is_constant: bool,
    /// Whether this symbol is a class (type, function, etc.)
    pub is_class: bool,
    /// Whether this symbol has a default/start value
    pub has_default: bool,
    /// The type name (Real, Integer, Boolean, String, or user-defined)
    pub type_name: String,
    /// Array dimensions (empty for scalars)
    pub shape: Vec<usize>,
    /// For functions: the return type (output variable type and shape)
    /// None for non-functions
    pub function_return: Option<(String, Vec<usize>)>,
}

impl DefinedSymbol {
    /// Create a new symbol for a component declaration
    pub fn from_component(name: &str, comp: &Component) -> (String, Self) {
        let line = comp
            .type_name
            .name
            .first()
            .map(|t| t.location.start_line)
            .unwrap_or(1);
        let col = comp
            .type_name
            .name
            .first()
            .map(|t| t.location.start_column)
            .unwrap_or(1);

        let has_start = !matches!(comp.start, Expression::Empty);
        let is_parameter = matches!(comp.variability, Variability::Parameter(_));
        let is_constant = matches!(comp.variability, Variability::Constant(_));
        let type_name = comp.type_name.to_string();

        (
            name.to_string(),
            Self {
                line,
                col,
                is_parameter,
                is_constant,
                is_class: false,
                has_default: has_start,
                type_name,
                shape: comp.shape.clone(),
                function_return: None,
            },
        )
    }

    /// Create a new symbol for a nested class/function
    pub fn from_class(name: &str, class: &ClassDefinition) -> (String, Self) {
        // For functions, extract return type from output components
        let function_return = if matches!(class.class_type, ClassType::Function) {
            class
                .components
                .values()
                .find(|c| matches!(c.causality, Causality::Output(_)))
                .map(|output| (output.type_name.to_string(), output.shape.clone()))
        } else {
            None
        };

        (
            name.to_string(),
            Self {
                line: class.name.location.start_line,
                col: class.name.location.start_column,
                is_parameter: false,
                is_constant: false,
                is_class: true,
                has_default: true,
                type_name: name.to_string(),
                shape: vec![],
                function_return,
            },
        )
    }

    /// Create a symbol for a loop index variable
    pub fn loop_index(line: u32, col: u32) -> Self {
        Self {
            line,
            col,
            is_parameter: false,
            is_constant: false,
            is_class: false,
            has_default: true,
            type_name: "Integer".to_string(),
            shape: vec![],
            function_return: None,
        }
    }
}

/// Add loop index variables to a defined symbols map.
///
/// This is a helper function to avoid code duplication when handling for loops
/// in the linter and LSP diagnostics. Each loop index is added as a locally
/// defined Integer variable.
pub fn add_loop_indices_to_defined(
    indices: &[ForIndex],
    defined: &mut HashMap<String, DefinedSymbol>,
) {
    for index in indices {
        defined.insert(
            index.ident.text.clone(),
            DefinedSymbol::loop_index(
                index.ident.location.start_line,
                index.ident.location.start_column,
            ),
        );
    }
}

/// Check if a type name represents a class instance (not a primitive type).
///
/// Returns `false` for built-in types like Real, Integer, Boolean, String,
/// and special types like StateSelect and ExternalObject.
pub fn is_class_instance_type(type_name: &str) -> bool {
    !matches!(
        type_name,
        "Real" | "Integer" | "Boolean" | "String" | "StateSelect" | "ExternalObject"
    )
}

/// Collect all defined symbols in a class.
///
/// This includes:
/// - Component declarations (variables, parameters, constants)
/// - Nested class definitions (functions, models, etc.)
pub fn collect_defined_symbols(class: &ClassDefinition) -> HashMap<String, DefinedSymbol> {
    let mut defined = HashMap::new();

    // Collect component declarations
    for (name, comp) in &class.components {
        let (sym_name, symbol) = DefinedSymbol::from_component(name, comp);
        defined.insert(sym_name, symbol);
    }

    // Collect nested class definitions
    for (name, nested_class) in &class.classes {
        let (sym_name, symbol) = DefinedSymbol::from_class(name, nested_class);
        defined.insert(sym_name, symbol);
    }

    defined
}

/// Collect all symbols used in a class (referenced in expressions, equations, etc.)
///
/// This function uses the [`SymbolCollectorVisitor`] internally for a clean,
/// maintainable implementation that leverages the visitor pattern.
pub fn collect_used_symbols(class: &ClassDefinition) -> HashSet<String> {
    let mut collector = SymbolCollectorVisitor::new();
    class.accept(&mut collector);
    collector.into_symbols()
}

// =============================================================================
// Visitor-based Symbol Collection
// =============================================================================

/// A visitor that collects all symbol references from the AST.
///
/// This visitor implements the [`Visitor`] trait and collects the first
/// identifier from every component reference encountered during traversal.
/// It provides a cleaner, more maintainable alternative to manual AST traversal.
///
/// ## Example
///
/// ```
/// use rumoca::ir::visitor::Visitable;
/// use rumoca::ir::analysis::symbols::SymbolCollectorVisitor;
/// use std::collections::HashSet;
///
/// // Create a collector and visit an AST node
/// let collector = SymbolCollectorVisitor::new();
/// // After calling: class_definition.accept(&mut collector);
/// let used_symbols: HashSet<String> = collector.into_symbols();
/// ```
pub struct SymbolCollectorVisitor {
    /// Collected symbol names
    symbols: HashSet<String>,
}

impl SymbolCollectorVisitor {
    /// Create a new symbol collector.
    pub fn new() -> Self {
        Self {
            symbols: HashSet::new(),
        }
    }

    /// Get the collected symbols as a reference.
    pub fn symbols(&self) -> &HashSet<String> {
        &self.symbols
    }

    /// Consume the visitor and return the collected symbols.
    pub fn into_symbols(self) -> HashSet<String> {
        self.symbols
    }
}

impl Default for SymbolCollectorVisitor {
    fn default() -> Self {
        Self::new()
    }
}

impl Visitor for SymbolCollectorVisitor {
    fn enter_component_reference(&mut self, node: &ComponentReference) {
        // Collect the first identifier from the component reference
        if let Some(first) = node.parts.first() {
            self.symbols.insert(first.ident.text.clone());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::modelica_grammar::ModelicaGrammar;
    use crate::modelica_parser::parse;

    fn parse_test_code(code: &str) -> crate::ir::ast::StoredDefinition {
        let mut grammar = ModelicaGrammar::new();
        parse(code, "test.mo", &mut grammar).expect("Failed to parse test code");
        grammar.modelica.expect("No AST produced")
    }

    #[test]
    fn test_collect_used_symbols_basic() {
        let code = r#"
model Test
  Real x;
  Real y;
equation
  x = y + 1.0;
  y = x * 2.0;
end Test;
"#;
        let ast = parse_test_code(code);
        let class = ast.class_list.get("Test").expect("Test class not found");

        let symbols = collect_used_symbols(class);

        assert!(symbols.contains("x"));
        assert!(symbols.contains("y"));
    }

    #[test]
    fn test_collect_used_symbols_with_for_loop() {
        let code = r#"
model Test
  Real x[10];
equation
  for i in 1:10 loop
    x[i] = i * 2.0;
  end for;
end Test;
"#;
        let ast = parse_test_code(code);
        let class = ast.class_list.get("Test").expect("Test class not found");

        let symbols = collect_used_symbols(class);

        assert!(symbols.contains("x"));
        assert!(symbols.contains("i"));
    }

    #[test]
    fn test_collect_used_symbols_with_function_call() {
        let code = r#"
model Test
  Real x;
  Real y;
equation
  x = sin(y);
  y = cos(x + 1.0);
end Test;
"#;
        let ast = parse_test_code(code);
        let class = ast.class_list.get("Test").expect("Test class not found");

        let symbols = collect_used_symbols(class);

        assert!(symbols.contains("x"));
        assert!(symbols.contains("y"));
        assert!(symbols.contains("sin"));
        assert!(symbols.contains("cos"));
    }

    #[test]
    fn test_symbol_collector_visitor_directly() {
        let code = r#"
model Test
  Real x;
  Real y;
equation
  x = y + 1.0;
end Test;
"#;
        let ast = parse_test_code(code);
        let class = ast.class_list.get("Test").expect("Test class not found");

        // Use the visitor directly
        let mut collector = SymbolCollectorVisitor::new();
        class.accept(&mut collector);

        assert!(collector.symbols().contains("x"));
        assert!(collector.symbols().contains("y"));

        let symbols = collector.into_symbols();
        assert!(symbols.contains("x"));
        assert!(symbols.contains("y"));
    }
}
