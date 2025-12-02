//! Visitor trait implementation for FormatVisitor.
//!
//! Implements the Visitor pattern for AST formatting.

use crate::ir::ast::{ClassDefinition, ClassType, StoredDefinition};
use crate::ir::visitor::Visitor;

use super::FormatVisitor;

/// Implement the Visitor trait for formatting
impl Visitor for FormatVisitor {
    fn enter_stored_definition(&mut self, node: &StoredDefinition) {
        // Within clause
        if let Some(within) = &node.within {
            let within_str = within.to_string();
            if within_str.is_empty() {
                self.writeln("within;");
            } else {
                self.writeln(&format!("within {};", within_str));
            }
            self.write("\n");
        }
    }

    fn enter_class_definition(&mut self, node: &ClassDefinition) {
        // Class header
        let class_keyword = match node.class_type {
            ClassType::Model => "model",
            ClassType::Class => "class",
            ClassType::Block => "block",
            ClassType::Connector => "connector",
            ClassType::Record => "record",
            ClassType::Type => "type",
            ClassType::Package => "package",
            ClassType::Function => "function",
            ClassType::Operator => "operator",
        };

        let encapsulated = if node.encapsulated {
            "encapsulated "
        } else {
            ""
        };
        // Format class header with optional description
        let description = if !node.description.is_empty() {
            let desc_strs: Vec<String> = node
                .description
                .iter()
                .map(|t| format!("\"{}\"", t.text))
                .collect();
            format!(" {}", desc_strs.join(" "))
        } else {
            String::new()
        };
        self.writeln(&format!(
            "{}{} {}{}",
            encapsulated, class_keyword, node.name.text, description
        ));
        self.indent_level += 1;

        // Extends
        for ext in &node.extends {
            self.writeln(&format!("extends {};", ext.comp));
        }

        // Imports
        for import in &node.imports {
            self.writeln(&self.format_import(import));
        }

        // Components (we handle these manually, not via visitor, to control ordering)
        for comp in node.components.values() {
            self.writeln(&self.format_component(comp));
        }

        // Nested classes - explicitly handle since we're not using accept() for format control
        for nested in node.classes.values() {
            self.enter_class_definition(nested);
            self.exit_class_definition(nested);
        }

        // Equations
        if !node.equations.is_empty() {
            self.indent_level -= 1;
            self.writeln("equation");
            self.indent_level += 1;
            for eq in &node.equations {
                let formatted = self.format_equation(eq, self.indent_level);
                self.write(&formatted);
            }
        }

        // Initial equations
        if !node.initial_equations.is_empty() {
            self.indent_level -= 1;
            self.writeln("initial equation");
            self.indent_level += 1;
            for eq in &node.initial_equations {
                let formatted = self.format_equation(eq, self.indent_level);
                self.write(&formatted);
            }
        }

        // Algorithms
        for algo in &node.algorithms {
            self.indent_level -= 1;
            self.writeln("algorithm");
            self.indent_level += 1;
            for stmt in algo {
                let formatted = self.format_statement(stmt, self.indent_level);
                self.write(&formatted);
            }
        }

        // Initial algorithms
        for algo in &node.initial_algorithms {
            self.indent_level -= 1;
            self.writeln("initial algorithm");
            self.indent_level += 1;
            for stmt in algo {
                let formatted = self.format_statement(stmt, self.indent_level);
                self.write(&formatted);
            }
        }
    }

    fn exit_class_definition(&mut self, node: &ClassDefinition) {
        self.indent_level -= 1;
        self.writeln(&format!("end {};", node.name.text));
    }
}
