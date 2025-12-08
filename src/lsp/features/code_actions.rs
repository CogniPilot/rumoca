//! Code Actions handler for Modelica files.
//!
//! Provides quick fixes and refactoring suggestions:
//! - Add missing parameter default value
//! - Remove unused variable
//! - Add missing semicolon (future)

use std::collections::HashMap;

use lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionParams, CodeActionResponse,
    Position, Range, TextEdit, Uri, WorkspaceEdit,
};

use crate::ir::ast::{Expression, Variability};

use crate::lsp::utils::parse_document;

/// Handle code action request
pub fn handle_code_action(
    documents: &HashMap<Uri, String>,
    params: CodeActionParams,
) -> Option<CodeActionResponse> {
    let uri = &params.text_document.uri;
    let text = documents.get(uri)?;
    let path = uri.path().as_str();
    let range = params.range;

    let mut actions = Vec::new();

    // Parse the document
    if let Some(ast) = parse_document(text, path) {
        for class in ast.class_list.values() {
            // Check for parameters without defaults
            for (comp_name, comp) in &class.components {
                if matches!(comp.variability, Variability::Parameter(_)) {
                    let has_default = !matches!(comp.start, Expression::Empty);

                    if !has_default {
                        // Get the component's location
                        let comp_line = comp
                            .type_name
                            .name
                            .first()
                            .map(|t| t.location.start_line.saturating_sub(1))
                            .unwrap_or(0);

                        // Check if this component is in the requested range
                        if comp_line >= range.start.line && comp_line <= range.end.line {
                            // Create a code action to add a default value
                            if let Some(action) =
                                create_add_default_action(uri, text, comp_name, comp_line)
                            {
                                actions.push(action);
                            }
                        }
                    }
                }
            }

            // Check for unused variables (variables that are defined but never used)
            // This would require more sophisticated analysis - for now, we provide
            // a remove action for any variable in the range
            for (comp_name, comp) in &class.components {
                let comp_line = comp
                    .type_name
                    .name
                    .first()
                    .map(|t| t.location.start_line.saturating_sub(1))
                    .unwrap_or(0);

                // Check if this component is in the requested range
                if comp_line >= range.start.line && comp_line <= range.end.line {
                    // Only offer remove for non-parameters (variables)
                    if !matches!(comp.variability, Variability::Parameter(_))
                        && let Some(action) =
                            create_remove_variable_action(uri, text, comp_name, comp_line)
                    {
                        actions.push(action);
                    }
                }
            }
        }
    }

    // Add quick fixes based on diagnostics in the range
    for diag in &params.context.diagnostics {
        if let Some(action) = create_quick_fix_for_diagnostic(uri, text, diag, &range) {
            actions.push(action);
        }
    }

    if actions.is_empty() {
        None
    } else {
        let response: CodeActionResponse = actions
            .into_iter()
            .map(CodeActionOrCommand::CodeAction)
            .collect();
        Some(response)
    }
}

/// Create a code action to add a default value to a parameter
fn create_add_default_action(
    uri: &Uri,
    text: &str,
    param_name: &str,
    line: u32,
) -> Option<CodeAction> {
    let lines: Vec<&str> = text.lines().collect();
    let line_text = lines.get(line as usize)?;

    // Find the position just before the semicolon
    let semicolon_pos = line_text.rfind(';')?;

    // Determine a sensible default based on position in line
    // For now, use 0 as a placeholder
    let default_text = " = 0";

    let mut changes = HashMap::new();
    changes.insert(
        uri.clone(),
        vec![TextEdit {
            range: Range {
                start: Position {
                    line,
                    character: semicolon_pos as u32,
                },
                end: Position {
                    line,
                    character: semicolon_pos as u32,
                },
            },
            new_text: default_text.to_string(),
        }],
    );

    Some(CodeAction {
        title: format!("Add default value to parameter '{}'", param_name),
        kind: Some(CodeActionKind::QUICKFIX),
        diagnostics: None,
        edit: Some(WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        }),
        command: None,
        is_preferred: Some(false),
        disabled: None,
        data: None,
    })
}

/// Create a code action to remove a variable declaration
fn create_remove_variable_action(
    uri: &Uri,
    text: &str,
    var_name: &str,
    line: u32,
) -> Option<CodeAction> {
    let lines: Vec<&str> = text.lines().collect();
    let _line_text = lines.get(line as usize)?;

    // Remove the entire line
    let mut changes = HashMap::new();
    changes.insert(
        uri.clone(),
        vec![TextEdit {
            range: Range {
                start: Position { line, character: 0 },
                end: Position {
                    line: line + 1,
                    character: 0,
                },
            },
            new_text: String::new(),
        }],
    );

    Some(CodeAction {
        title: format!("Remove unused variable '{}'", var_name),
        kind: Some(CodeActionKind::QUICKFIX),
        diagnostics: None,
        edit: Some(WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        }),
        command: None,
        is_preferred: Some(false),
        disabled: None,
        data: None,
    })
}

/// Create a quick fix based on a diagnostic message
fn create_quick_fix_for_diagnostic(
    uri: &Uri,
    text: &str,
    diagnostic: &lsp_types::Diagnostic,
    _range: &Range,
) -> Option<CodeAction> {
    let message = &diagnostic.message;

    // Handle "Undefined variable" - suggest adding a declaration
    if message.starts_with("Undefined variable:") {
        let var_name = message
            .strip_prefix("Undefined variable: '")?
            .strip_suffix('\'')?;
        return create_declare_variable_action(uri, text, var_name, diagnostic);
    }

    // Handle "Unused variable" - offer to prefix with underscore
    if message.starts_with("Unused variable:") {
        let var_name = message
            .strip_prefix("Unused variable: '")?
            .strip_suffix("' is declared but never used")?;
        return create_prefix_underscore_action(uri, var_name, diagnostic);
    }

    // Handle "Parameter without default"
    if message.contains("has no default value") {
        return create_add_default_from_diagnostic(uri, text, diagnostic);
    }

    None
}

/// Create a code action to declare an undefined variable
fn create_declare_variable_action(
    uri: &Uri,
    text: &str,
    var_name: &str,
    diagnostic: &lsp_types::Diagnostic,
) -> Option<CodeAction> {
    let lines: Vec<&str> = text.lines().collect();

    // Find the equation section to insert before
    let mut insert_line = 0u32;
    for (i, line) in lines.iter().enumerate() {
        let trimmed = line.trim();
        if trimmed == "equation" || trimmed.starts_with("equation ") {
            insert_line = i as u32;
            break;
        }
    }

    if insert_line == 0 {
        return None;
    }

    // Get indentation from the equation line
    let indent = lines
        .get(insert_line as usize)
        .map(|l| l.len() - l.trim_start().len())
        .unwrap_or(2);
    let indent_str = " ".repeat(indent);

    let declaration = format!("{}Real {};\n", indent_str, var_name);

    let mut changes = HashMap::new();
    changes.insert(
        uri.clone(),
        vec![TextEdit {
            range: Range {
                start: Position {
                    line: insert_line,
                    character: 0,
                },
                end: Position {
                    line: insert_line,
                    character: 0,
                },
            },
            new_text: declaration,
        }],
    );

    Some(CodeAction {
        title: format!("Declare variable '{}' as Real", var_name),
        kind: Some(CodeActionKind::QUICKFIX),
        diagnostics: Some(vec![diagnostic.clone()]),
        edit: Some(WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        }),
        command: None,
        is_preferred: Some(true),
        disabled: None,
        data: None,
    })
}

/// Create a code action to prefix an unused variable with underscore
fn create_prefix_underscore_action(
    uri: &Uri,
    var_name: &str,
    diagnostic: &lsp_types::Diagnostic,
) -> Option<CodeAction> {
    let line = diagnostic.range.start.line;
    let start_char = diagnostic.range.start.character;
    let end_char = diagnostic.range.end.character;

    let new_name = format!("_{}", var_name);

    let mut changes = HashMap::new();
    changes.insert(
        uri.clone(),
        vec![TextEdit {
            range: Range {
                start: Position {
                    line,
                    character: start_char,
                },
                end: Position {
                    line,
                    character: end_char,
                },
            },
            new_text: new_name.clone(),
        }],
    );

    Some(CodeAction {
        title: format!("Rename to '{}' to suppress warning", new_name),
        kind: Some(CodeActionKind::QUICKFIX),
        diagnostics: Some(vec![diagnostic.clone()]),
        edit: Some(WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        }),
        command: None,
        is_preferred: Some(false),
        disabled: None,
        data: None,
    })
}

/// Create a code action to add default value from a diagnostic
fn create_add_default_from_diagnostic(
    uri: &Uri,
    text: &str,
    diagnostic: &lsp_types::Diagnostic,
) -> Option<CodeAction> {
    let line = diagnostic.range.start.line;
    let lines: Vec<&str> = text.lines().collect();
    let line_text = lines.get(line as usize)?;

    // Find the semicolon
    let semicolon_pos = line_text.rfind(';')?;

    let mut changes = HashMap::new();
    changes.insert(
        uri.clone(),
        vec![TextEdit {
            range: Range {
                start: Position {
                    line,
                    character: semicolon_pos as u32,
                },
                end: Position {
                    line,
                    character: semicolon_pos as u32,
                },
            },
            new_text: " = 0".to_string(),
        }],
    );

    Some(CodeAction {
        title: "Add default value".to_string(),
        kind: Some(CodeActionKind::QUICKFIX),
        diagnostics: Some(vec![diagnostic.clone()]),
        edit: Some(WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        }),
        command: None,
        is_preferred: Some(true),
        disabled: None,
        data: None,
    })
}
