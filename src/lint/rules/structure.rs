//! Structural lint rules.
//!
//! Rules for checking class structure, parameters, empty sections, and extends.

use std::collections::HashSet;

use crate::ir::ast::{ClassDefinition, ClassType, Expression, Variability};
use crate::lint::{LintLevel, LintMessage, LintResult};

/// Check for missing documentation
pub fn lint_missing_documentation(
    _class: &ClassDefinition,
    _file_path: &str,
    _result: &mut LintResult,
) {
    // ClassDefinition doesn't have a description field currently
    // This lint is a placeholder for future enhancement when class-level
    // documentation strings are captured by the parser
}

/// Check for parameters without default values
pub fn lint_parameter_defaults(class: &ClassDefinition, file_path: &str, result: &mut LintResult) {
    for (name, comp) in &class.components {
        if matches!(comp.variability, Variability::Parameter(_)) {
            let has_default = !matches!(comp.start, Expression::Empty);
            if !has_default {
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

                result.messages.push(
                    LintMessage::new(
                        "parameter-no-default",
                        LintLevel::Help,
                        format!("Parameter '{}' has no default value", name),
                        file_path,
                        line,
                        col,
                    )
                    .with_suggestion("Consider adding a default value for better usability"),
                );
            }
        }
    }
}

/// Check for empty sections
pub fn lint_empty_sections(class: &ClassDefinition, file_path: &str, result: &mut LintResult) {
    let line = class.name.location.start_line;

    // Check for models/blocks without equations (might be intentional for partial classes)
    if matches!(class.class_type, ClassType::Model | ClassType::Block)
        && class.equations.is_empty()
        && class.initial_equations.is_empty()
        && class.algorithms.is_empty()
        && class.initial_algorithms.is_empty()
        && class.extends.is_empty() // Not inherited
        && !class.components.is_empty()
    // Has components but no equations
    {
        result.messages.push(LintMessage::new(
            "empty-section",
            LintLevel::Note,
            format!(
                "{} '{}' has components but no equations or algorithms",
                format_class_type(&class.class_type),
                class.name.text
            ),
            file_path,
            line,
            1,
        ));
    }
}

/// Check for unit consistency (simplified check)
pub fn lint_unit_consistency(class: &ClassDefinition, file_path: &str, result: &mut LintResult) {
    // This is a simplified check - real unit analysis would require more infrastructure
    // For now, we just check if components have unit attributes

    let mut has_units = false;
    let mut missing_units = Vec::new();

    for (name, comp) in &class.components {
        // Skip non-Real types
        if comp.type_name.to_string() != "Real" {
            continue;
        }

        // Check if unit modifier is present (simplified - would need modifier parsing)
        // For now, just note that this lint exists
        let has_unit = false; // Placeholder - would check comp.modifiers

        if has_unit {
            has_units = true;
        } else {
            missing_units.push((
                name.clone(),
                comp.type_name
                    .name
                    .first()
                    .map(|t| t.location.start_line)
                    .unwrap_or(1),
            ));
        }
    }

    // If some variables have units but others don't, warn about inconsistency
    if has_units && !missing_units.is_empty() {
        for (name, line) in missing_units {
            result.messages.push(LintMessage::new(
                "inconsistent-units",
                LintLevel::Warning,
                format!(
                    "Variable '{}' has no unit specification while others do",
                    name
                ),
                file_path,
                line,
                1,
            ));
        }
    }
}

/// Check for redundant extends
pub fn lint_redundant_extends(class: &ClassDefinition, file_path: &str, result: &mut LintResult) {
    let line = class.name.location.start_line;

    // Check for duplicate extends
    let mut seen_extends: HashSet<String> = HashSet::new();
    for ext in &class.extends {
        let ext_name = ext.comp.to_string();
        if seen_extends.contains(&ext_name) {
            result.messages.push(LintMessage::new(
                "redundant-extends",
                LintLevel::Warning,
                format!("Duplicate extends clause for '{}'", ext_name),
                file_path,
                line,
                1,
            ));
        }
        seen_extends.insert(ext_name);
    }

    // Check if extending self (would cause infinite recursion)
    for ext in &class.extends {
        if ext.comp.to_string() == class.name.text {
            result.messages.push(LintMessage::new(
                "redundant-extends",
                LintLevel::Error,
                format!("Class '{}' extends itself", class.name.text),
                file_path,
                line,
                1,
            ));
        }
    }
}

fn format_class_type(ct: &ClassType) -> &'static str {
    match ct {
        ClassType::Model => "Model",
        ClassType::Class => "Class",
        ClassType::Block => "Block",
        ClassType::Connector => "Connector",
        ClassType::Record => "Record",
        ClassType::Type => "Type",
        ClassType::Function => "Function",
        ClassType::Package => "Package",
        ClassType::Operator => "Operator",
    }
}
