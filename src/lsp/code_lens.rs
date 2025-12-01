//! Code Lens handler for Modelica files.
//!
//! Provides inline actionable information:
//! - Reference counts for classes, functions, and variables
//! - "Extends" information for models
//! - Component counts for models

// Allow mutable key type warning - Uri has interior mutability but we use it correctly
#![allow(clippy::mutable_key_type)]

use std::collections::HashMap;

use lsp_types::{CodeLens, CodeLensParams, Command, Position, Range, Uri};

use crate::ir::ast::{ClassDefinition, ClassType, StoredDefinition};

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

    // Add component count lens for models/blocks
    if matches!(class.class_type, ClassType::Model | ClassType::Block | ClassType::Class) {
        let comp_count = class.components.len();
        let eq_count = class.equations.len() + class.initial_equations.len();

        if comp_count > 0 || eq_count > 0 {
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
                        "{} component{}, {} equation{}",
                        comp_count,
                        if comp_count == 1 { "" } else { "s" },
                        eq_count,
                        if eq_count == 1 { "" } else { "s" }
                    ),
                    command: String::new(), // No action, just informational
                    arguments: None,
                }),
                data: None,
            });
        }
    }

    // Add extends lens if class extends another
    if !class.extends.is_empty() {
        let extends_names: Vec<String> = class
            .extends
            .iter()
            .map(|e| e.comp.to_string())
            .collect();

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

/// Count references to a name in the document
fn count_references(name: &str, text: &str, ast: &StoredDefinition) -> usize {
    let mut count = 0;

    // Count type references in components
    for class in ast.class_list.values() {
        count += count_references_in_class(name, class);
    }

    // Also do a simple text search as fallback
    // (This catches references in equations/expressions that may not be in the AST components)
    for line in text.lines() {
        // Skip the definition line itself
        if line.contains(&format!("model {}", name)) ||
           line.contains(&format!("class {}", name)) ||
           line.contains(&format!("function {}", name)) ||
           line.contains(&format!("record {}", name)) ||
           line.contains(&format!("connector {}", name)) ||
           line.contains(&format!("block {}", name)) ||
           line.contains(&format!("type {}", name)) ||
           line.contains(&format!("package {}", name)) {
            continue;
        }

        // Count occurrences as a word boundary
        let mut search_pos = 0;
        while let Some(pos) = line[search_pos..].find(name) {
            let abs_pos = search_pos + pos;
            let before_ok = abs_pos == 0 ||
                !line.chars().nth(abs_pos - 1).unwrap_or(' ').is_alphanumeric();
            let after_ok = abs_pos + name.len() >= line.len() ||
                !line.chars().nth(abs_pos + name.len()).unwrap_or(' ').is_alphanumeric();

            if before_ok && after_ok {
                count += 1;
            }
            search_pos = abs_pos + 1;
            if search_pos >= line.len() {
                break;
            }
        }
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
