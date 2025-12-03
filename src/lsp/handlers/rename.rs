//! Rename Symbol handler for Modelica files.
//!
//! Provides rename refactoring support:
//! - Rename variables, parameters, components
//! - Rename classes, functions, records
//! - Updates all references in the document
//! - Workspace-wide rename across multiple files

use std::collections::HashMap;

use lsp_types::{
    Position, PrepareRenameResponse, Range, RenameParams, TextDocumentPositionParams, TextEdit,
    Uri, WorkspaceEdit,
};

use crate::ir::ast::{ClassDefinition, Component, ComponentReference, StoredDefinition, Token};
use crate::ir::visitor::{Visitable, Visitor};

use crate::lsp::utils::{get_word_at_position, parse_document, token_to_range};
use crate::lsp::workspace::WorkspaceState;

/// Visitor that finds all occurrences of a symbol for renaming
struct SymbolOccurrenceFinder<'a> {
    /// The old symbol name to find
    old_name: &'a str,
    /// The new name to replace with
    new_name: &'a str,
    /// Collected text edits
    edits: Vec<TextEdit>,
}

impl<'a> SymbolOccurrenceFinder<'a> {
    fn new(old_name: &'a str, new_name: &'a str) -> Self {
        Self {
            old_name,
            new_name,
            edits: Vec::new(),
        }
    }

    fn add_edit_from_token(&mut self, token: &Token) {
        self.edits.push(TextEdit {
            range: token_to_range(token),
            new_text: self.new_name.to_string(),
        });
    }
}

impl Visitor for SymbolOccurrenceFinder<'_> {
    fn enter_class_definition(&mut self, node: &ClassDefinition) {
        // Check class name
        if node.name.text == self.old_name {
            self.add_edit_from_token(&node.name);

            // Also rename "end ClassName"
            if let Some(end_name) = &node.end_name_token {
                self.add_edit_from_token(end_name);
            }
        }

        // Check nested class names
        for (nested_name, nested_class) in &node.classes {
            if nested_name == self.old_name {
                self.add_edit_from_token(&nested_class.name);
            }
        }
    }

    fn enter_component(&mut self, node: &Component) {
        // Check component name
        if node.name_token.text == self.old_name {
            self.add_edit_from_token(&node.name_token);
        }

        // Check type name references
        for token in &node.type_name.name {
            if token.text == self.old_name {
                self.add_edit_from_token(token);
            }
        }
    }

    fn enter_component_reference(&mut self, node: &ComponentReference) {
        for part in &node.parts {
            if part.ident.text == self.old_name {
                self.add_edit_from_token(&part.ident);
            }
        }
    }
}

/// Handle prepare rename request - validates if rename is possible at this location
pub fn handle_prepare_rename(
    documents: &HashMap<Uri, String>,
    params: TextDocumentPositionParams,
) -> Option<PrepareRenameResponse> {
    let uri = &params.text_document.uri;
    let position = params.position;

    let text = documents.get(uri)?;
    let path = uri.path().as_str();

    let word = get_word_at_position(text, position)?;
    let ast = parse_document(text, path)?;

    // Check if the word is a renameable symbol
    if is_renameable_symbol(&ast, &word) {
        // Find the exact range of the word
        let lines: Vec<&str> = text.lines().collect();
        let line = lines.get(position.line as usize)?;
        let col = position.character as usize;

        // Find word boundaries
        let start = line[..col]
            .rfind(|c: char| !c.is_alphanumeric() && c != '_')
            .map(|i| i + 1)
            .unwrap_or(0);

        let end = line[col..]
            .find(|c: char| !c.is_alphanumeric() && c != '_')
            .map(|i| col + i)
            .unwrap_or(line.len());

        Some(PrepareRenameResponse::Range(Range {
            start: Position {
                line: position.line,
                character: start as u32,
            },
            end: Position {
                line: position.line,
                character: end as u32,
            },
        }))
    } else {
        None
    }
}

/// Handle rename request - performs the actual rename
pub fn handle_rename(
    documents: &HashMap<Uri, String>,
    params: RenameParams,
) -> Option<WorkspaceEdit> {
    let uri = &params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;
    let new_name = &params.new_name;

    let text = documents.get(uri)?;
    let path = uri.path().as_str();

    let old_name = get_word_at_position(text, position)?;
    let ast = parse_document(text, path)?;

    // Verify this is a renameable symbol
    if !is_renameable_symbol(&ast, &old_name) {
        return None;
    }

    // Find all occurrences using the visitor
    let mut finder = SymbolOccurrenceFinder::new(&old_name, new_name);
    ast.accept(&mut finder);

    if finder.edits.is_empty() {
        return None;
    }

    let mut edits = finder.edits;

    // Sort edits by position (reverse order for safe application)
    edits.sort_by(|a, b| {
        let line_cmp = b.range.start.line.cmp(&a.range.start.line);
        if line_cmp == std::cmp::Ordering::Equal {
            b.range.start.character.cmp(&a.range.start.character)
        } else {
            line_cmp
        }
    });

    // Remove duplicates (same range)
    edits.dedup_by(|a, b| a.range == b.range);

    let mut changes = HashMap::new();
    changes.insert(uri.clone(), edits);

    Some(WorkspaceEdit {
        changes: Some(changes),
        document_changes: None,
        change_annotations: None,
    })
}

/// Check if a symbol can be renamed
fn is_renameable_symbol(def: &StoredDefinition, name: &str) -> bool {
    for class in def.class_list.values() {
        if is_symbol_in_class(class, name) {
            return true;
        }
    }
    false
}

/// Check if a symbol exists in a class (as component, class name, or nested class)
fn is_symbol_in_class(class: &ClassDefinition, name: &str) -> bool {
    // Check if it's the class name itself
    if class.name.text == name {
        return true;
    }

    // Check components
    if class.components.contains_key(name) {
        return true;
    }

    // Check nested classes
    if class.classes.contains_key(name) {
        return true;
    }

    // Recursively check nested classes
    for nested in class.classes.values() {
        if is_symbol_in_class(nested, name) {
            return true;
        }
    }

    false
}

/// Handle rename with workspace support - renames across all open files
pub fn handle_rename_workspace(
    workspace: &WorkspaceState,
    params: RenameParams,
) -> Option<WorkspaceEdit> {
    let uri = &params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;
    let new_name = &params.new_name;

    let text = workspace.get_document(uri)?;
    let path = uri.path().as_str();

    let old_name = get_word_at_position(text, position)?;
    let ast = parse_document(text, path)?;

    // Verify this is a renameable symbol
    if !is_renameable_symbol(&ast, &old_name) {
        return None;
    }

    let mut all_changes: HashMap<Uri, Vec<TextEdit>> = HashMap::new();

    // Collect edits for all open documents
    for (doc_uri, doc_text) in workspace.documents() {
        let doc_path = doc_uri.path().as_str();
        if let Some(doc_ast) = parse_document(doc_text, doc_path) {
            let mut finder = SymbolOccurrenceFinder::new(&old_name, new_name);
            doc_ast.accept(&mut finder);

            if !finder.edits.is_empty() {
                let mut edits = finder.edits;

                // Sort edits by position (reverse order for safe application)
                edits.sort_by(|a, b| {
                    let line_cmp = b.range.start.line.cmp(&a.range.start.line);
                    if line_cmp == std::cmp::Ordering::Equal {
                        b.range.start.character.cmp(&a.range.start.character)
                    } else {
                        line_cmp
                    }
                });

                // Remove duplicates (same range)
                edits.dedup_by(|a, b| a.range == b.range);

                all_changes.insert(doc_uri.clone(), edits);
            }
        }
    }

    if all_changes.is_empty() {
        None
    } else {
        Some(WorkspaceEdit {
            changes: Some(all_changes),
            document_changes: None,
            change_annotations: None,
        })
    }
}
