//! Symbol table for variable scoping and resolution
//!
//! This module provides a symbol table implementation for tracking variables
//! during the flattening process. It maintains a mapping from local names
//! (as they appear in expressions) to their qualified names (as they should
//! appear in the flattened output).

use crate::ir::analysis::symbol_trait::SymbolInfo;
use crate::ir::analysis::type_inference::{SymbolType, type_from_name};
use crate::ir::transform::constants::global_builtins;
use indexmap::IndexMap;
use std::collections::HashSet;

/// Represents a symbol in the symbol table
#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    /// The local name (as it appears in expressions)
    pub local_name: String,
    /// The fully qualified name (e.g., "e1_x" for component e1's variable x)
    pub qualified_name: String,
    /// The type name of the symbol
    pub type_name: String,
    /// Whether this symbol is a parameter
    pub is_parameter: bool,
}

impl SymbolInfo for Symbol {
    fn name(&self) -> &str {
        &self.local_name
    }

    fn qualified_name(&self) -> &str {
        &self.qualified_name
    }

    fn symbol_type(&self) -> SymbolType {
        type_from_name(&self.type_name)
    }

    fn line(&self) -> u32 {
        0 // Symbol doesn't track line information
    }

    fn column(&self) -> u32 {
        0 // Symbol doesn't track column information
    }

    fn is_parameter(&self) -> bool {
        self.is_parameter
    }
}

/// Symbol table for tracking variable scopes during flattening
#[derive(Debug, Clone)]
pub struct SymbolTable {
    /// Map from local name to symbol information
    symbols: IndexMap<String, Symbol>,
    /// Set of global/built-in symbols that don't need qualification
    global_symbols: HashSet<String>,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolTable {
    /// Create a new symbol table with built-in globals
    pub fn new() -> Self {
        // Use the centralized list of global builtins from constants module
        let global_symbols: HashSet<String> = global_builtins().into_iter().collect();

        Self {
            symbols: IndexMap::new(),
            global_symbols,
        }
    }

    /// Add a symbol to the table
    pub fn add_symbol(
        &mut self,
        local_name: &str,
        qualified_name: &str,
        type_name: &str,
        is_parameter: bool,
    ) {
        self.symbols.insert(
            local_name.to_string(),
            Symbol {
                local_name: local_name.to_string(),
                qualified_name: qualified_name.to_string(),
                type_name: type_name.to_string(),
                is_parameter,
            },
        );
    }

    /// Check if a name is a global/built-in symbol
    pub fn is_global(&self, name: &str) -> bool {
        self.global_symbols.contains(name)
    }

    /// Add a global symbol (e.g., user-defined function name)
    pub fn add_global(&mut self, name: &str) {
        self.global_symbols.insert(name.to_string());
    }

    /// Remove a global symbol (e.g., when exiting a for loop scope)
    pub fn remove_global(&mut self, name: &str) {
        self.global_symbols.remove(name);
    }

    /// Remove a symbol by name (from either local or global symbols)
    pub fn remove(&mut self, name: &str) {
        self.symbols.shift_remove(name);
        self.global_symbols.remove(name);
    }

    /// Look up a symbol by its local name
    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }

    /// Check if a symbol exists (either local or global)
    pub fn contains(&self, name: &str) -> bool {
        self.symbols.contains_key(name) || self.global_symbols.contains(name)
    }

    /// Get the qualified name for a local name, or None if not found
    pub fn get_qualified_name(&self, local_name: &str) -> Option<&str> {
        self.symbols
            .get(local_name)
            .map(|s| s.qualified_name.as_str())
    }

    /// Get all symbols in the table
    pub fn symbols(&self) -> &IndexMap<String, Symbol> {
        &self.symbols
    }

    /// Check if any symbol starts with the given prefix followed by a dot
    /// This is used to validate references like "D.x" when "D.x_start" is defined
    pub fn has_prefix(&self, prefix: &str) -> bool {
        let prefix_dot = format!("{}.", prefix);
        self.symbols.keys().any(|k| k.starts_with(&prefix_dot))
    }

    /// Clear all non-global symbols
    pub fn clear(&mut self) {
        self.symbols.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_table_basics() {
        let mut table = SymbolTable::new();

        // Add a symbol
        table.add_symbol("x", "e1_x", "Real", false);

        // Check lookup
        assert!(table.contains("x"));
        let sym = table.lookup("x").unwrap();
        assert_eq!(sym.qualified_name, "e1_x");
        assert_eq!(sym.type_name, "Real");
        assert!(!sym.is_parameter);

        // Check global
        assert!(table.is_global("time"));
        assert!(table.is_global("der"));
        assert!(!table.is_global("x"));

        // Check contains for both
        assert!(table.contains("time"));
        assert!(table.contains("x"));
        assert!(!table.contains("unknown"));
    }

    #[test]
    fn test_qualified_name_lookup() {
        let mut table = SymbolTable::new();
        table.add_symbol("x", "comp_x", "Real", false);
        table.add_symbol("k", "comp_k", "Real", true);

        assert_eq!(table.get_qualified_name("x"), Some("comp_x"));
        assert_eq!(table.get_qualified_name("k"), Some("comp_k"));
        assert_eq!(table.get_qualified_name("unknown"), None);
    }
}
