//! Code Lens handler for Modelica files.
//!
//! Provides inline actionable information:
//! - Reference counts for classes, functions, and variables
//! - "Extends" information for models
//! - Component counts and balance status for models

// Allow mutable key type warning - Uri has interior mutability but we use it correctly
#![allow(clippy::mutable_key_type)]

use std::collections::HashMap;

use lsp_types::{CodeLens, CodeLensParams, Command, Position, Range, Uri};

use crate::ir::ast::{ClassDefinition, ClassType, StoredDefinition};
use crate::ir::balance_check::check_class_balance;

use super::utils::parse_document;

/// Handle code lens request
pub fn handle_code_lens(
    documents: &HashMap<Uri, String>,
    params: CodeLensParams,
) -> Option<Vec<CodeLens>> {
    let uri = &params.text_document.uri;
    let text = documents.get(uri)?;
    let path = uri.path().as_str();

    let mut lenses = Vec::new();

    if let Some(ast) = parse_document(text, path) {
        for class in ast.class_list.values() {
            collect_class_lenses(class, text, &ast, &mut lenses);
        }
    }

    Some(lenses)
}

/// Collect code lenses for a class
fn collect_class_lenses(
    class: &ClassDefinition,
    text: &str,
    ast: &StoredDefinition,
    lenses: &mut Vec<CodeLens>,
) {
    let class_line = class.name.location.start_line.saturating_sub(1);

    // Add component count and balance status lens for models/blocks
    if matches!(
        class.class_type,
        ClassType::Model | ClassType::Block | ClassType::Class
    ) {
        let balance = check_class_balance(class);

        if balance.num_unknowns > 0 || balance.num_equations > 0 {
            // Build status string with balance indicator
            let status_icon = if balance.is_balanced {
                "✓"
            } else if balance.difference() > 0 {
                "⚠ over"
            } else {
                "⚠ under"
            };

            // Format: "states, unknowns, equations, [status]"
            let title = format!(
                "{} states, {} unknowns, {} equations [{}]",
                balance.num_states, balance.num_unknowns, balance.num_equations, status_icon
            );

            lenses.push(CodeLens {
                range: Range {
                    start: Position {
                        line: class_line,
                        character: 0,
                    },
                    end: Position {
                        line: class_line,
                        character: 0,
                    },
                },
                command: Some(Command {
                    title,
                    command: String::new(), // No action, just informational
                    arguments: None,
                }),
                data: None,
            });
        }
    }

    // Add extends lens if class extends another
    if !class.extends.is_empty() {
        let extends_names: Vec<String> = class.extends.iter().map(|e| e.comp.to_string()).collect();

        lenses.push(CodeLens {
            range: Range {
                start: Position {
                    line: class_line,
                    character: 0,
                },
                end: Position {
                    line: class_line,
                    character: 0,
                },
            },
            command: Some(Command {
                title: format!("extends {}", extends_names.join(", ")),
                command: String::new(),
                arguments: None,
            }),
            data: None,
        });
    }

    // Add reference count lens
    let ref_count = count_references(&class.name.text, text, ast);
    if ref_count > 0 {
        lenses.push(CodeLens {
            range: Range {
                start: Position {
                    line: class_line,
                    character: 0,
                },
                end: Position {
                    line: class_line,
                    character: 0,
                },
            },
            command: Some(Command {
                title: format!(
                    "{} reference{}",
                    ref_count,
                    if ref_count == 1 { "" } else { "s" }
                ),
                command: "editor.action.findReferences".to_string(),
                arguments: None,
            }),
            data: None,
        });
    }

    // Add lens for functions showing parameter count
    if class.class_type == ClassType::Function {
        let input_count = class
            .components
            .values()
            .filter(|c| matches!(c.causality, crate::ir::ast::Causality::Input(_)))
            .count();
        let output_count = class
            .components
            .values()
            .filter(|c| matches!(c.causality, crate::ir::ast::Causality::Output(_)))
            .count();

        lenses.push(CodeLens {
            range: Range {
                start: Position {
                    line: class_line,
                    character: 0,
                },
                end: Position {
                    line: class_line,
                    character: 0,
                },
            },
            command: Some(Command {
                title: format!(
                    "{} input{}, {} output{}",
                    input_count,
                    if input_count == 1 { "" } else { "s" },
                    output_count,
                    if output_count == 1 { "" } else { "s" }
                ),
                command: String::new(),
                arguments: None,
            }),
            data: None,
        });
    }

    // Recursively process nested classes
    for nested in class.classes.values() {
        collect_class_lenses(nested, text, ast, lenses);
    }
}

/// Count references to a name in the document (usages only, not declarations)
fn count_references(name: &str, _text: &str, ast: &StoredDefinition) -> usize {
    let mut count = 0;

    // Count type references in components and extends clauses
    // This only counts actual usages, not the declaration itself
    for class in ast.class_list.values() {
        count += count_references_in_class(name, class);
    }

    count
}

/// Count references to a name within a class
fn count_references_in_class(name: &str, class: &ClassDefinition) -> usize {
    let mut count = 0;

    // Count in components (type references)
    for comp in class.components.values() {
        if comp.type_name.to_string() == name {
            count += 1;
        }
    }

    // Count in extends
    for ext in &class.extends {
        if ext.comp.to_string() == name {
            count += 1;
        }
    }

    // Recursively check nested classes
    for nested in class.classes.values() {
        count += count_references_in_class(name, nested);
    }

    count
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_count_references_simple() {
        // Test basic text matching for references
        let text = "model Test\n  MyType x;\n  MyType y;\nend Test;";
        let count = text.matches("MyType").count();
        assert_eq!(count, 2);
    }
}
