//! Component formatting for the format visitor.
//!
//! Provides formatting methods for component declarations.

use crate::ir::ast::{Causality, Component, Connection, Expression, Variability};

use super::FormatVisitor;

impl FormatVisitor {
    /// Check if a component has individual attributes that prevent grouping
    pub fn component_has_individual_attrs(&self, comp: &Component) -> bool {
        // Has modifications (e.g., R=10)
        if !comp.modifications.is_empty() {
            return true;
        }
        // Has description string
        if !comp.description.is_empty() {
            return true;
        }
        // Has annotation
        if !comp.annotation.is_empty() {
            return true;
        }
        // Has conditional component (if condition)
        if comp.condition.is_some() {
            return true;
        }
        // Has inner/outer prefix
        if comp.inner || comp.outer {
            return true;
        }
        // Has start value as modification (start=x)
        if comp.start_is_modification && !matches!(comp.start, Expression::Empty) {
            return true;
        }
        // Has start value as binding with explicit source location
        if !comp.start_is_modification {
            if let Expression::Terminal { token, .. } = &comp.start {
                if token.location.start_line > 0 {
                    return true;
                }
            } else if !matches!(comp.start, Expression::Empty)
                && comp.start.get_location().is_some_and(|l| l.start_line > 0)
            {
                return true;
            }
        }
        false
    }

    /// Format a group of components that can be combined on one line
    /// Returns the formatted string like "Real x, y, z;"
    pub fn format_component_group(&self, components: &[&Component]) -> String {
        if components.is_empty() {
            return String::new();
        }

        let first = components[0];
        let mut result = String::new();

        // Variability prefix (same for all in group)
        match &first.variability {
            Variability::Constant(_) => result.push_str("constant "),
            Variability::Parameter(_) => result.push_str("parameter "),
            Variability::Discrete(_) => result.push_str("discrete "),
            Variability::Empty => {}
        }

        // Causality prefix (same for all in group)
        match &first.causality {
            Causality::Input(_) => result.push_str("input "),
            Causality::Output(_) => result.push_str("output "),
            Causality::Empty => {}
        }

        // Connection prefix (same for all in group)
        match &first.connection {
            Connection::Flow(_) => result.push_str("flow "),
            Connection::Stream(_) => result.push_str("stream "),
            Connection::Empty => {}
        }

        // Type name (same for all in group)
        result.push_str(&first.type_name.to_string());
        result.push(' ');

        // Component names with their array dimensions
        let names: Vec<String> = components
            .iter()
            .map(|comp| {
                let mut name = comp.name.clone();
                // Prefer shape_expr (original subscripts) over shape (evaluated integers)
                if !comp.shape_expr.is_empty() {
                    let dims: Vec<String> = comp
                        .shape_expr
                        .iter()
                        .map(|s| self.format_subscript(s))
                        .collect();
                    name.push_str(&format!("[{}]", dims.join(", ")));
                } else if !comp.shape.is_empty() {
                    let dims: Vec<String> = comp.shape.iter().map(|d| d.to_string()).collect();
                    name.push_str(&format!("[{}]", dims.join(", ")));
                }
                name
            })
            .collect();
        result.push_str(&names.join(", "));
        result.push(';');
        result
    }

    pub fn format_component(&self, comp: &Component) -> String {
        let mut result = String::new();

        // Inner/outer prefix (must come first)
        if comp.inner {
            result.push_str("inner ");
        }
        if comp.outer {
            result.push_str("outer ");
        }

        // Variability prefix
        match &comp.variability {
            Variability::Constant(_) => result.push_str("constant "),
            Variability::Parameter(_) => result.push_str("parameter "),
            Variability::Discrete(_) => result.push_str("discrete "),
            Variability::Empty => {}
        }

        // Causality prefix
        match &comp.causality {
            Causality::Input(_) => result.push_str("input "),
            Causality::Output(_) => result.push_str("output "),
            Causality::Empty => {}
        }

        // Connection prefix
        match &comp.connection {
            Connection::Flow(_) => result.push_str("flow "),
            Connection::Stream(_) => result.push_str("stream "),
            Connection::Empty => {}
        }

        // Type name
        result.push_str(&comp.type_name.to_string());
        result.push(' ');

        // Component name
        result.push_str(&comp.name);

        // Array dimensions - prefer shape_expr (original subscripts) over shape (evaluated integers)
        if !comp.shape_expr.is_empty() {
            let dims: Vec<String> = comp
                .shape_expr
                .iter()
                .map(|s| self.format_subscript(s))
                .collect();
            result.push_str(&format!("[{}]", dims.join(", ")));
        } else if !comp.shape.is_empty() {
            let dims: Vec<String> = comp.shape.iter().map(|d| d.to_string()).collect();
            result.push_str(&format!("[{}]", dims.join(", ")));
        }

        // Modifications and start value as modification (inside parentheses)
        let has_start_mod = comp.start_is_modification && !matches!(comp.start, Expression::Empty);
        if !comp.modifications.is_empty() || has_start_mod {
            let mut mods: Vec<String> = Vec::new();
            // Add start= modifier if it's a modification
            if has_start_mod {
                let each_prefix = if comp.start_has_each { "each " } else { "" };
                mods.push(format!(
                    "{}start = {}",
                    each_prefix,
                    self.format_expression(&comp.start)
                ));
            }
            // Add other modifications
            for (k, v) in &comp.modifications {
                mods.push(format!("{} = {}", k, self.format_expression(v)));
            }
            result.push_str(&format!("({})", mods.join(", ")));
        }

        // Start value as binding equation (= value) - only if not a modification
        // and has an explicit source location (default values set by parser have empty locations)
        if !comp.start_is_modification {
            if let Expression::Terminal { token, .. } = &comp.start {
                if token.location.start_line > 0 {
                    result.push_str(&format!(" = {}", self.format_expression(&comp.start)));
                }
            } else if !matches!(comp.start, Expression::Empty) {
                // Non-terminal expressions (like arrays) should always be output
                if comp.start.get_location().is_some_and(|l| l.start_line > 0) {
                    result.push_str(&format!(" = {}", self.format_expression(&comp.start)));
                }
            }
        }

        // Conditional component (if condition) - comes before description per Modelica spec
        if let Some(cond) = &comp.condition {
            result.push_str(&format!(" if {}", self.format_expression(cond)));
        }

        // Description string
        if !comp.description.is_empty() {
            let desc: Vec<String> = comp
                .description
                .iter()
                .map(|t| format!("\"{}\"", t.text))
                .collect();
            result.push_str(&format!(" {}", desc.join(" ")));
        }

        // Annotation
        if !comp.annotation.is_empty() {
            let args: Vec<String> = comp
                .annotation
                .iter()
                .map(|e| self.format_expression(e))
                .collect();
            result.push_str(&format!(" annotation({})", args.join(", ")));
        }

        result.push(';');
        result
    }
}
