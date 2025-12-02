//! Go to definition handler for Modelica files.
//!
//! Supports:
//! - Local definitions (variables, parameters, nested classes)
//! - Cross-file definitions (via workspace state)

use std::collections::HashMap;

use lsp_types::{GotoDefinitionParams, GotoDefinitionResponse, Location, Position, Range, Uri};

use crate::ir::ast::{ClassDefinition, StoredDefinition, Token};

use super::utils::{get_word_at_position, token_to_range};
use super::workspace::WorkspaceState;

/// Handle go to definition request
pub fn handle_goto_definition(
    documents: &HashMap<Uri, String>,
    params: GotoDefinitionParams,
) -> Option<GotoDefinitionResponse> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    let text = documents.get(uri)?;
    let path = uri.path().as_str();

    let word = get_word_at_position(text, position)?;

    if let Ok(result) = crate::Compiler::new().compile_str(text, path) {
        if let Some(token) = find_definition_in_ast(&result.def, &word) {
            return Some(GotoDefinitionResponse::Scalar(Location {
                uri: uri.clone(),
                range: token_to_range(token),
            }));
        }
    }

    None
}

/// Handle go to definition with workspace support for cross-file navigation
pub fn handle_goto_definition_workspace(
    workspace: &WorkspaceState,
    params: GotoDefinitionParams,
) -> Option<GotoDefinitionResponse> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    let text = workspace.get_document(uri)?;
    let path = uri.path().as_str();

    let word = get_word_at_position(text, position)?;

    // First try local definition
    if let Ok(result) = crate::Compiler::new().compile_str(text, path) {
        if let Some(token) = find_definition_in_ast(&result.def, &word) {
            return Some(GotoDefinitionResponse::Scalar(Location {
                uri: uri.clone(),
                range: token_to_range(token),
            }));
        }
    }

    // Try workspace-wide symbol lookup
    // First check if word is a qualified name or simple name
    if let Some(sym) = workspace.lookup_symbol(&word) {
        return Some(GotoDefinitionResponse::Scalar(Location {
            uri: sym.uri.clone(),
            range: Range {
                start: Position {
                    line: sym.line,
                    character: sym.column,
                },
                end: Position {
                    line: sym.line,
                    character: sym.column + word.len() as u32,
                },
            },
        }));
    }

    // Try looking up by simple name (last part of qualified name)
    let simple_name = word.rsplit('.').next().unwrap_or(&word);
    let matches = workspace.lookup_by_simple_name(simple_name);
    if matches.len() == 1 {
        let sym = matches[0];
        return Some(GotoDefinitionResponse::Scalar(Location {
            uri: sym.uri.clone(),
            range: Range {
                start: Position {
                    line: sym.line,
                    character: sym.column,
                },
                end: Position {
                    line: sym.line,
                    character: sym.column + simple_name.len() as u32,
                },
            },
        }));
    } else if matches.len() > 1 {
        // Multiple matches - return all of them
        let locations: Vec<Location> = matches
            .iter()
            .map(|sym| Location {
                uri: sym.uri.clone(),
                range: Range {
                    start: Position {
                        line: sym.line,
                        character: sym.column,
                    },
                    end: Position {
                        line: sym.line,
                        character: sym.column + simple_name.len() as u32,
                    },
                },
            })
            .collect();
        return Some(GotoDefinitionResponse::Array(locations));
    }

    None
}

/// Find a definition in the AST, returning the Token if found
fn find_definition_in_ast<'a>(def: &'a StoredDefinition, name: &str) -> Option<&'a Token> {
    for class in def.class_list.values() {
        if let Some(token) = find_definition_in_class(class, name) {
            return Some(token);
        }
    }
    None
}

/// Recursively search for a definition in a class
fn find_definition_in_class<'a>(class: &'a ClassDefinition, name: &str) -> Option<&'a Token> {
    if class.name.text == name {
        return Some(&class.name);
    }

    for (comp_name, comp) in &class.components {
        if comp_name == name {
            return Some(&comp.name_token);
        }
    }

    for nested_class in class.classes.values() {
        if let Some(token) = find_definition_in_class(nested_class, name) {
            return Some(token);
        }
    }

    None
}
