//! Class formatting with comment preservation.

use super::visitor::FormatVisitor;
use crate::ir::ast::{Causality, ClassDefinition, ClassType, Component, Equation, Statement};

/// Get the source line number of an equation
pub fn get_equation_location(eq: &Equation) -> Option<u32> {
    match eq {
        Equation::Empty => None,
        Equation::Simple { lhs, .. } => lhs.get_location().map(|l| l.start_line),
        Equation::Connect { lhs, .. } => Some(lhs.parts.first()?.ident.location.start_line),
        Equation::For { indices, .. } => Some(indices.first()?.ident.location.start_line),
        Equation::When(blocks) => blocks
            .first()
            .and_then(|b| b.cond.get_location())
            .map(|l| l.start_line),
        Equation::If { cond_blocks, .. } => cond_blocks
            .first()
            .and_then(|b| b.cond.get_location())
            .map(|l| l.start_line),
        Equation::FunctionCall { comp, .. } => Some(comp.parts.first()?.ident.location.start_line),
    }
}

/// Get the source line number of a statement
pub fn get_statement_location(stmt: &Statement) -> Option<u32> {
    match stmt {
        Statement::Empty => None,
        Statement::Assignment { comp, .. } => Some(comp.parts.first()?.ident.location.start_line),
        Statement::FunctionCall { comp, .. } => Some(comp.parts.first()?.ident.location.start_line),
        Statement::For { indices, .. } => Some(indices.first()?.ident.location.start_line),
        Statement::While(block) => block.cond.get_location().map(|l| l.start_line),
        Statement::If { cond_blocks, .. } => cond_blocks
            .first()
            .and_then(|b| b.cond.get_location())
            .map(|l| l.start_line),
        Statement::When(blocks) => blocks
            .first()
            .and_then(|b| b.cond.get_location())
            .map(|l| l.start_line),
        Statement::Return { token } => Some(token.location.start_line),
        Statement::Break { token } => Some(token.location.start_line),
    }
}

/// Check if a class is a short class definition (type alias)
/// e.g., `connector RealInput = input Real;`
fn is_short_class_definition(class: &ClassDefinition) -> bool {
    // Short class definitions have no end_name_token and typically:
    // - Have exactly one extends clause
    // - Have no components, equations, algorithms, or nested classes
    class.end_name_token.is_none()
        && class.extends.len() == 1
        && class.components.is_empty()
        && class.equations.is_empty()
        && class.initial_equations.is_empty()
        && class.algorithms.is_empty()
        && class.initial_algorithms.is_empty()
        && class.classes.is_empty()
}

/// Format a class definition with comment insertion
///
/// `add_trailing_blanks` - if true, adds blank lines after this class ends (for spacing between classes)
pub fn format_class_with_comments(
    visitor: &mut FormatVisitor,
    class: &ClassDefinition,
    add_trailing_blanks: bool,
) {
    // Get the class's starting line from its name token
    let class_line = class.name.location.start_line;

    // Emit any comments that should appear before this class
    visitor.emit_comments_before_line(class_line);

    // Check for short class definition (type alias) like `connector RealInput = input Real;`
    if is_short_class_definition(class) {
        let class_keyword = match class.class_type {
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

        let causality_prefix = match &class.causality {
            Causality::Input(_) => "input ",
            Causality::Output(_) => "output ",
            Causality::Empty => "",
        };

        let ext = &class.extends[0];
        let base_type = &ext.comp;
        // Include modifications if present (e.g., Real(unit="s"))
        let mods_str = if !ext.modifications.is_empty() {
            let mod_strs: Vec<String> = ext
                .modifications
                .iter()
                .map(|e| visitor.format_expression(e))
                .collect();
            format!("({})", mod_strs.join(", "))
        } else {
            String::new()
        };
        visitor.writeln(&format!(
            "{} {} = {}{}{};",
            class_keyword, class.name.text, causality_prefix, base_type, mods_str
        ));

        // Add blank lines after this class if requested
        if add_trailing_blanks {
            for _ in 0..visitor.options.blank_lines_between_classes {
                visitor.write("\n");
            }
        }
        return;
    }

    // Format the class header
    let class_keyword = match class.class_type {
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

    let encapsulated = if class.encapsulated {
        "encapsulated "
    } else {
        ""
    };
    // Format class header with optional description
    let description = if !class.description.is_empty() {
        let desc_strs: Vec<String> = class
            .description
            .iter()
            .map(|t| format!("\"{}\"", t.text))
            .collect();
        format!(" {}", desc_strs.join(" "))
    } else {
        String::new()
    };
    visitor.writeln(&format!(
        "{}{} {}{}",
        encapsulated, class_keyword, class.name.text, description
    ));
    visitor.indent_level += 1;

    // Extends
    for ext in &class.extends {
        let ext_line = ext.location.start_line;
        visitor.emit_comments_before_line(ext_line);
        visitor.writeln(&format!("extends {};", ext.comp));
    }

    // Imports
    for import in &class.imports {
        let import_line = import.location().start_line;
        visitor.emit_comments_before_line(import_line);
        visitor.writeln(&visitor.format_import(import));
    }

    // Components - group by source line to preserve combined declarations like "Real x, y, z;"
    let components: Vec<&Component> = class.components.values().collect();
    let mut i = 0;
    while i < components.len() {
        let comp = components[i];
        let comp_line = comp.location.start_line;
        visitor.emit_comments_before_line(comp_line);

        // Check if this component can be grouped with following ones
        // Components can be grouped if:
        // 1. They're on the same source line
        // 2. They have the same type, variability, causality, and connection
        // 3. None of them have individual attributes (descriptions, annotations, start values, modifications)
        if !visitor.component_has_individual_attrs(comp) {
            let mut group: Vec<&Component> = vec![comp];
            let mut j = i + 1;
            while j < components.len() {
                let next = components[j];
                if next.location.start_line == comp_line
                    && next.type_name == comp.type_name
                    && std::mem::discriminant(&next.variability)
                        == std::mem::discriminant(&comp.variability)
                    && std::mem::discriminant(&next.causality)
                        == std::mem::discriminant(&comp.causality)
                    && std::mem::discriminant(&next.connection)
                        == std::mem::discriminant(&comp.connection)
                    && !visitor.component_has_individual_attrs(next)
                {
                    group.push(next);
                    j += 1;
                } else {
                    break;
                }
            }

            if group.len() > 1 {
                // Output as a grouped declaration with trailing comments
                let formatted = visitor.format_component_group(&group);
                visitor.writeln_with_trailing(&formatted, comp_line);
                i = j;
                continue;
            }
        }

        // Output as individual declaration with trailing comments
        let formatted = visitor.format_component(comp);
        visitor.writeln_with_trailing(&formatted, comp_line);
        i += 1;
    }

    // Nested classes
    let nested_count = class.classes.len();
    for (i, nested) in class.classes.values().enumerate() {
        let is_last_nested = i == nested_count - 1;
        format_class_with_comments(visitor, nested, !is_last_nested);
    }

    // Equations
    if !class.equations.is_empty() {
        // Find first equation's line for comment insertion
        if let Some(first_eq) = class.equations.first()
            && let Some(loc) = get_equation_location(first_eq)
        {
            // Emit comments before the equation keyword (approximate)
            visitor.emit_comments_before_line(loc.saturating_sub(1));
        }
        visitor.indent_level -= 1;
        visitor.writeln("equation");
        visitor.indent_level += 1;
        for eq in &class.equations {
            if let Some(eq_line) = get_equation_location(eq) {
                visitor.emit_comments_before_line(eq_line);
            }
            let formatted = visitor.format_equation(eq, visitor.indent_level);
            visitor.write(&formatted);
        }
    }

    // Initial equations
    if !class.initial_equations.is_empty() {
        if let Some(first_eq) = class.initial_equations.first()
            && let Some(loc) = get_equation_location(first_eq)
        {
            visitor.emit_comments_before_line(loc.saturating_sub(1));
        }
        visitor.indent_level -= 1;
        visitor.writeln("initial equation");
        visitor.indent_level += 1;
        for eq in &class.initial_equations {
            if let Some(eq_line) = get_equation_location(eq) {
                visitor.emit_comments_before_line(eq_line);
            }
            let formatted = visitor.format_equation(eq, visitor.indent_level);
            visitor.write(&formatted);
        }
    }

    // Algorithms
    for algo in &class.algorithms {
        visitor.indent_level -= 1;
        visitor.writeln("algorithm");
        visitor.indent_level += 1;
        for stmt in algo {
            if let Some(stmt_line) = get_statement_location(stmt) {
                visitor.emit_comments_before_line(stmt_line);
            }
            let formatted = visitor.format_statement(stmt, visitor.indent_level);
            visitor.write(&formatted);
        }
    }

    // Initial algorithms
    for algo in &class.initial_algorithms {
        visitor.indent_level -= 1;
        visitor.writeln("initial algorithm");
        visitor.indent_level += 1;
        for stmt in algo {
            if let Some(stmt_line) = get_statement_location(stmt) {
                visitor.emit_comments_before_line(stmt_line);
            }
            let formatted = visitor.format_statement(stmt, visitor.indent_level);
            visitor.write(&formatted);
        }
    }

    // End class - emit any remaining comments for this class before end
    visitor.indent_level -= 1;
    visitor.writeln(&format!("end {};", class.name.text));

    // Add blank lines after this class if requested (for spacing between classes)
    if add_trailing_blanks {
        for _ in 0..visitor.options.blank_lines_between_classes {
            visitor.write("\n");
        }
    }
}
