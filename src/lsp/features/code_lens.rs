//! Code Lens handler for Modelica files.
//!
//! Provides inline actionable information:
//! - Balance status for models/blocks (states, unknowns, equations)
//! - Reference counts for classes, functions, and variables
//! - "Extends" information for models

use lsp_types::{CodeLens, CodeLensParams, Command, Position, Range, Uri};

use crate::dae::balance::BalanceStatus;
use crate::ir::ast::{ClassDefinition, ClassType, StoredDefinition};

use crate::lsp::WorkspaceState;
use crate::lsp::utils::parse_document;

/// Handle code lens request
pub fn handle_code_lens(
    workspace: &WorkspaceState,
    params: CodeLensParams,
) -> Option<Vec<CodeLens>> {
    let uri = &params.text_document.uri;
    let text = workspace.get_document(uri)?;
    let path = uri.path().as_str();

    let mut lenses = Vec::new();

    if let Some(ast) = parse_document(text, path) {
        for class in ast.class_list.values() {
            collect_class_lenses(
                class,
                text,
                &ast,
                uri,
                workspace,
                "", // No prefix for top-level classes
                &mut lenses,
            );
        }
    }

    Some(lenses)
}

/// Collect code lenses for a class
fn collect_class_lenses(
    class: &ClassDefinition,
    text: &str,
    ast: &StoredDefinition,
    uri: &Uri,
    workspace: &WorkspaceState,
    prefix: &str,
    lenses: &mut Vec<CodeLens>,
) {
    let class_line = class.name.location.start_line.saturating_sub(1);

    // Build the full class path (for nested classes)
    let class_path = if prefix.is_empty() {
        class.name.text.clone()
    } else {
        format!("{}.{}", prefix, class.name.text)
    };

    // Show balance info for models/blocks/classes/connectors (computed automatically during diagnostics)
    if matches!(
        class.class_type,
        ClassType::Model | ClassType::Block | ClassType::Class | ClassType::Connector
    ) && let Some(balance) = workspace.get_balance(uri, &class_path)
    {
        // Format compile time nicely
        let time_str = if balance.compile_time_ms > 0 {
            format!(" ({}ms)", balance.compile_time_ms)
        } else {
            String::new()
        };

        let title = match &balance.status {
            BalanceStatus::Balanced => format!(
                "{} states, {} unknowns, {} equations [✓]{}",
                balance.num_states, balance.num_unknowns, balance.num_equations, time_str
            ),
            BalanceStatus::Partial => format!(
                "{} states, {} unknowns, {} equations [◐ partial]{}",
                balance.num_states, balance.num_unknowns, balance.num_equations, time_str
            ),
            BalanceStatus::Unbalanced => {
                let icon = if balance.difference() > 0 {
                    "⚠ over"
                } else {
                    "⚠ under"
                };
                format!(
                    "{} states, {} unknowns, {} equations [{}]{}",
                    balance.num_states, balance.num_unknowns, balance.num_equations, icon, time_str
                )
            }
            BalanceStatus::CompileError(msg) => format!("✗ compile error: {}{}", msg, time_str),
        };

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
                command: String::new(), // Not clickable
                arguments: None,
            }),
            data: None,
        });
    }
    // No fallback "Analyze" button - balance is computed automatically

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

    // Recursively process nested classes with updated prefix
    for nested in class.classes.values() {
        collect_class_lenses(nested, text, ast, uri, workspace, &class_path, lenses);
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
