//! Variable reference validator
//!
//! This visitor validates that all variable references in expressions
//! correspond to declared components using a SymbolTable.

use crate::ir::analysis::symbol_table::SymbolTable;
use crate::ir::ast::{ClassDefinition, ComponentReference, Expression, Import, Variability};
use crate::ir::visitor::MutVisitor;
use std::collections::HashSet;

/// Visitor that validates all variable references exist
pub struct VarValidator {
    /// Symbol table for tracking declared variables
    symbol_table: SymbolTable,
    /// Imported package root names (e.g., "Modelica" from "import Modelica;")
    imported_packages: HashSet<String>,
    /// Undefined variables found
    pub undefined_vars: Vec<(String, String)>, // (var_name, context)
}

impl VarValidator {
    pub fn new(class: &ClassDefinition) -> Self {
        Self::with_context(class, &[], &[])
    }

    /// Create a validator with additional function names that should be considered valid
    pub fn with_functions(class: &ClassDefinition, function_names: &[String]) -> Self {
        Self::with_context(class, function_names, &[])
    }

    /// Create a validator with both function names and peer class names
    pub fn with_context(
        class: &ClassDefinition,
        function_names: &[String],
        peer_class_names: &[String],
    ) -> Self {
        let mut symbol_table = SymbolTable::new();

        // Add function names as global symbols
        for name in function_names {
            symbol_table.add_global(name);
        }

        // Add peer class names as global symbols (for cross-class type references)
        // This allows references like `SwitchController.SwitchState` from another class
        for name in peer_class_names {
            symbol_table.add_global(name);
        }

        // Collect all declared component names
        for (name, comp) in &class.components {
            let is_parameter = matches!(comp.variability, Variability::Parameter(_));
            symbol_table.add_symbol(name, name, &comp.type_name.to_string(), is_parameter);
        }

        // Add nested class names as global symbols (includes types and enumerations)
        // This allows references like `State.Off` where `State` is a nested type definition
        for name in class.classes.keys() {
            symbol_table.add_global(name);
        }

        // Collect imported package root names from the class's imports
        let imported_packages = Self::collect_imported_packages(&class.imports);

        Self {
            symbol_table,
            imported_packages,
            undefined_vars: Vec::new(),
        }
    }

    /// Collect the root package names from imports.
    ///
    /// For example:
    /// - `import Modelica;` -> "Modelica"
    /// - `import Modelica.Math.*;` -> "Modelica"
    /// - `import SI = Modelica.Units.SI;` -> "Modelica" (the actual package root)
    fn collect_imported_packages(imports: &[Import]) -> HashSet<String> {
        let mut packages = HashSet::new();

        for import in imports {
            match import {
                Import::Qualified { path, .. } => {
                    // import A.B.C; -> root is "A"
                    if let Some(first) = path.name.first() {
                        packages.insert(first.text.clone());
                    }
                }
                Import::Renamed { path, .. } => {
                    // import D = A.B.C; -> root is "A"
                    if let Some(first) = path.name.first() {
                        packages.insert(first.text.clone());
                    }
                }
                Import::Unqualified { path, .. } => {
                    // import A.B.*; -> root is "A"
                    if let Some(first) = path.name.first() {
                        packages.insert(first.text.clone());
                    }
                }
                Import::Selective { path, .. } => {
                    // import A.B.{C, D}; -> root is "A"
                    if let Some(first) = path.name.first() {
                        packages.insert(first.text.clone());
                    }
                }
            }
        }

        packages
    }

    fn check_component_ref(&mut self, comp_ref: &ComponentReference, context: &str) {
        // Build the full qualified name from all parts
        let full_name = comp_ref.to_string();

        // Check the first part of the reference
        if let Some(first_part) = comp_ref.parts.first() {
            let first_name = &first_part.ident.text;

            // Skip validation if any of these are true:
            // 1. The first part is in the symbol table (declared variable or built-in)
            // 2. The full qualified name is in the symbol table (e.g., "D.x_start")
            // 3. The first part is an imported package root (e.g., "Modelica")
            // 4. There's a component that starts with this prefix (e.g., "D" when "D.x" exists)
            if self.symbol_table.contains(first_name)
                || self.symbol_table.contains(&full_name)
                || self.imported_packages.contains(first_name)
                || self.symbol_table.has_prefix(first_name)
            {
                return;
            }

            self.undefined_vars
                .push((first_name.clone(), context.to_string()));
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
