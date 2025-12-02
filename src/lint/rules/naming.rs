//! Naming convention lint rules.
//!
//! Rules for checking variable, class, and component naming conventions.

use crate::ir::ast::ClassDefinition;
use crate::lint::{LintLevel, LintMessage, LintResult, is_class_instance_type};

/// Check naming conventions
pub fn lint_naming_conventions(class: &ClassDefinition, file_path: &str, result: &mut LintResult) {
    let class_name = &class.name.text;
    let line = class.name.location.start_line;
    let col = class.name.location.start_column;

    // Class names should be CamelCase (start with uppercase)
    if !class_name
        .chars()
        .next()
        .map(|c| c.is_uppercase())
        .unwrap_or(false)
    {
        result.messages.push(
            LintMessage::new(
                "naming-convention",
                LintLevel::Note,
                format!(
                    "Class name '{}' should start with an uppercase letter (CamelCase)",
                    class_name
                ),
                file_path,
                line,
                col,
            )
            .with_suggestion(format!("Rename to '{}'", capitalize_first(class_name))),
        );
    }

    // Check component names (should be camelCase or snake_case for variables)
    for (comp_name, comp) in &class.components {
        let comp_line = comp
            .type_name
            .name
            .first()
            .map(|t| t.location.start_line)
            .unwrap_or(1);
        let comp_col = comp
            .type_name
            .name
            .first()
            .map(|t| t.location.start_column)
            .unwrap_or(1);

        // Variable/parameter names should not start with uppercase (unless it's a type instance)
        let is_type_instance = is_class_instance_type(&comp.type_name.to_string());
        if !is_type_instance
            && comp_name
                .chars()
                .next()
                .map(|c| c.is_uppercase())
                .unwrap_or(false)
        {
            result.messages.push(
                LintMessage::new(
                    "naming-convention",
                    LintLevel::Note,
                    format!(
                        "Variable '{}' should start with a lowercase letter",
                        comp_name
                    ),
                    file_path,
                    comp_line,
                    comp_col,
                )
                .with_suggestion(format!("Rename to '{}'", lowercase_first(comp_name))),
            );
        }

        // Single-letter names are discouraged except for common ones
        let allowed_single = [
            "x", "y", "z", "t", "u", "v", "w", "i", "j", "k", "n", "m", "p", "q", "r", "s",
        ];
        if comp_name.len() == 1 && !allowed_single.contains(&comp_name.as_str()) {
            result.messages.push(LintMessage::new(
                "naming-convention",
                LintLevel::Help,
                format!("Single-letter variable name '{}' may be unclear", comp_name),
                file_path,
                comp_line,
                comp_col,
            ));
        }
    }
}

fn capitalize_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(c) => c.to_uppercase().chain(chars).collect(),
    }
}

fn lowercase_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(c) => c.to_lowercase().chain(chars).collect(),
    }
}
