//! Variable reference validator
//!
//! This visitor validates that all variable references in expressions
//! correspond to declared components using a SymbolTable.

use crate::ir::analysis::symbol_table::SymbolTable;
use crate::ir::ast::{ClassDefinition, ComponentReference, Expression, Variability};
use crate::ir::visitor::MutVisitor;

/// Visitor that validates all variable references exist
pub struct VarValidator {
    /// Symbol table for tracking declared variables
    symbol_table: SymbolTable,
    /// Undefined variables found
    pub undefined_vars: Vec<(String, String)>, // (var_name, context)
}

impl VarValidator {
    pub fn new(class: &ClassDefinition) -> Self {
        Self::with_functions(class, &[])
    }

    /// Create a validator with additional function names that should be considered valid
    pub fn with_functions(class: &ClassDefinition, function_names: &[String]) -> Self {
        let mut symbol_table = SymbolTable::new();

        // Add function names as global symbols
        for name in function_names {
            symbol_table.add_global(name);
        }

        // Collect all declared component names
        for (name, comp) in &class.components {
            let is_parameter = matches!(comp.variability, Variability::Parameter(_));
            symbol_table.add_symbol(name, name, &comp.type_name.to_string(), is_parameter);
        }

        Self {
            symbol_table,
            undefined_vars: Vec::new(),
        }
    }

    fn check_component_ref(&mut self, comp_ref: &ComponentReference, context: &str) {
        // For now, just check the first part (simple variable names)
        // TODO: Handle hierarchical references properly
        if let Some(first_part) = comp_ref.parts.first() {
            let var_name = &first_part.ident.text;
            if !self.symbol_table.contains(var_name) {
                self.undefined_vars
                    .push((var_name.clone(), context.to_string()));
            }
        }
    }
}

impl MutVisitor for VarValidator {
    fn enter_expression(&mut self, expr: &mut Expression) {
        match expr {
            Expression::ComponentReference(comp_ref) => {
                self.check_component_ref(comp_ref, "expression");
            }
            Expression::FunctionCall { comp, .. } => {
                self.check_component_ref(comp, "function call");
            }
            _ => {}
        }
    }
}
