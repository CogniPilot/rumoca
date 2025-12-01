//! Rename Symbol handler for Modelica files.
//!
//! Provides rename refactoring support:
//! - Rename variables, parameters, components
//! - Rename classes, functions, records
//! - Updates all references in the document
//! - Workspace-wide rename across multiple files

// Allow mutable key type warning - Uri has interior mutability but we use it correctly
#![allow(clippy::mutable_key_type)]

use std::collections::HashMap;

use lsp_types::{
    Position, PrepareRenameResponse, Range, RenameParams, TextDocumentPositionParams, TextEdit,
    Uri, WorkspaceEdit,
};

use crate::ir::ast::{
    ClassDefinition, ComponentReference, Equation, Expression, Statement, StoredDefinition,
    Subscript,
};

use super::utils::{get_word_at_position, parse_document};
use super::workspace::WorkspaceState;

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

    // Find all occurrences of the symbol
    let mut edits = Vec::new();
    collect_symbol_occurrences(&ast, &old_name, text, &mut edits, new_name);

    if edits.is_empty() {
        return None;
    }

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

/// Collect all occurrences of a symbol in the AST
fn collect_symbol_occurrences(
    def: &StoredDefinition,
    old_name: &str,
    text: &str,
    edits: &mut Vec<TextEdit>,
    new_name: &str,
) {
    for class in def.class_list.values() {
        collect_occurrences_in_class(class, old_name, text, edits, new_name);
    }
}

/// Collect symbol occurrences in a class
fn collect_occurrences_in_class(
    class: &ClassDefinition,
    old_name: &str,
    text: &str,
    edits: &mut Vec<TextEdit>,
    new_name: &str,
) {
    // Check class name
    if class.name.text == old_name {
        add_edit_at_location(
            class.name.location.start_line,
            class.name.location.start_column,
            old_name,
            new_name,
            edits,
        );

        // Also find "end ClassName" pattern
        find_end_class_name(text, old_name, new_name, edits);
    }

    // Check components
    for (comp_name, comp) in &class.components {
        if comp_name == old_name {
            // Component declaration - find in text using type_name location as anchor
            if let Some(first_token) = comp.type_name.name.first() {
                // The component name comes after the type name on the same line usually
                let line = first_token.location.start_line;
                find_identifier_on_line(text, line, old_name, new_name, edits);
            }
        }

        // Check type name references
        for token in &comp.type_name.name {
            if token.text == old_name {
                add_edit_at_location(
                    token.location.start_line,
                    token.location.start_column,
                    old_name,
                    new_name,
                    edits,
                );
            }
        }

        // Check start expression
        collect_occurrences_in_expression(&comp.start, old_name, new_name, edits);
    }

    // Check equations
    for eq in &class.equations {
        collect_occurrences_in_equation(eq, old_name, new_name, edits);
    }

    // Check initial equations
    for eq in &class.initial_equations {
        collect_occurrences_in_equation(eq, old_name, new_name, edits);
    }

    // Check algorithms
    for algo in &class.algorithms {
        for stmt in algo {
            collect_occurrences_in_statement(stmt, old_name, new_name, edits);
        }
    }

    // Check initial algorithms
    for algo in &class.initial_algorithms {
        for stmt in algo {
            collect_occurrences_in_statement(stmt, old_name, new_name, edits);
        }
    }

    // Check nested classes
    for (nested_name, nested_class) in &class.classes {
        if nested_name == old_name {
            add_edit_at_location(
                nested_class.name.location.start_line,
                nested_class.name.location.start_column,
                old_name,
                new_name,
                edits,
            );
        }
        collect_occurrences_in_class(nested_class, old_name, text, edits, new_name);
    }
}

/// Collect occurrences in an equation
fn collect_occurrences_in_equation(
    eq: &Equation,
    old_name: &str,
    new_name: &str,
    edits: &mut Vec<TextEdit>,
) {
    match eq {
        Equation::Empty => {}
        Equation::Simple { lhs, rhs } => {
            collect_occurrences_in_expression(lhs, old_name, new_name, edits);
            collect_occurrences_in_expression(rhs, old_name, new_name, edits);
        }
        Equation::Connect { lhs, rhs } => {
            collect_occurrences_in_component_ref(lhs, old_name, new_name, edits);
            collect_occurrences_in_component_ref(rhs, old_name, new_name, edits);
        }
        Equation::For { indices, equations } => {
            for index in indices {
                collect_occurrences_in_expression(&index.range, old_name, new_name, edits);
            }
            for sub_eq in equations {
                collect_occurrences_in_equation(sub_eq, old_name, new_name, edits);
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                collect_occurrences_in_expression(&block.cond, old_name, new_name, edits);
                for eq in &block.eqs {
                    collect_occurrences_in_equation(eq, old_name, new_name, edits);
                }
            }
            if let Some(else_eqs) = else_block {
                for eq in else_eqs {
                    collect_occurrences_in_equation(eq, old_name, new_name, edits);
                }
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                collect_occurrences_in_expression(&block.cond, old_name, new_name, edits);
                for eq in &block.eqs {
                    collect_occurrences_in_equation(eq, old_name, new_name, edits);
                }
            }
        }
        Equation::FunctionCall { comp, args } => {
            collect_occurrences_in_component_ref(comp, old_name, new_name, edits);
            for arg in args {
                collect_occurrences_in_expression(arg, old_name, new_name, edits);
            }
        }
    }
}

/// Collect occurrences in a statement
fn collect_occurrences_in_statement(
    stmt: &Statement,
    old_name: &str,
    new_name: &str,
    edits: &mut Vec<TextEdit>,
) {
    match stmt {
        Statement::Empty => {}
        Statement::Assignment { comp, value } => {
            collect_occurrences_in_component_ref(comp, old_name, new_name, edits);
            collect_occurrences_in_expression(value, old_name, new_name, edits);
        }
        Statement::For { indices, equations } => {
            for index in indices {
                collect_occurrences_in_expression(&index.range, old_name, new_name, edits);
            }
            for sub_stmt in equations {
                collect_occurrences_in_statement(sub_stmt, old_name, new_name, edits);
            }
        }
        Statement::While(block) => {
            collect_occurrences_in_expression(&block.cond, old_name, new_name, edits);
            for stmt in &block.stmts {
                collect_occurrences_in_statement(stmt, old_name, new_name, edits);
            }
        }
        Statement::Return { .. } => {}
        Statement::Break { .. } => {}
        Statement::FunctionCall { comp, args } => {
            collect_occurrences_in_component_ref(comp, old_name, new_name, edits);
            for arg in args {
                collect_occurrences_in_expression(arg, old_name, new_name, edits);
            }
        }
    }
}

/// Collect occurrences in an expression
fn collect_occurrences_in_expression(
    expr: &Expression,
    old_name: &str,
    new_name: &str,
    edits: &mut Vec<TextEdit>,
) {
    match expr {
        Expression::Empty => {}
        Expression::ComponentReference(comp_ref) => {
            collect_occurrences_in_component_ref(comp_ref, old_name, new_name, edits);
        }
        Expression::Terminal { .. } => {}
        Expression::Binary { lhs, op: _, rhs } => {
            collect_occurrences_in_expression(lhs, old_name, new_name, edits);
            collect_occurrences_in_expression(rhs, old_name, new_name, edits);
        }
        Expression::Unary { op: _, rhs } => {
            collect_occurrences_in_expression(rhs, old_name, new_name, edits);
        }
        Expression::FunctionCall { comp, args } => {
            collect_occurrences_in_component_ref(comp, old_name, new_name, edits);
            for arg in args {
                collect_occurrences_in_expression(arg, old_name, new_name, edits);
            }
        }
        Expression::Array { elements } => {
            for elem in elements {
                collect_occurrences_in_expression(elem, old_name, new_name, edits);
            }
        }
        Expression::Tuple { elements } => {
            for elem in elements {
                collect_occurrences_in_expression(elem, old_name, new_name, edits);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                collect_occurrences_in_expression(cond, old_name, new_name, edits);
                collect_occurrences_in_expression(then_expr, old_name, new_name, edits);
            }
            collect_occurrences_in_expression(else_branch, old_name, new_name, edits);
        }
        Expression::Range { start, step, end } => {
            collect_occurrences_in_expression(start, old_name, new_name, edits);
            if let Some(step) = step {
                collect_occurrences_in_expression(step, old_name, new_name, edits);
            }
            collect_occurrences_in_expression(end, old_name, new_name, edits);
        }
    }
}

/// Collect occurrences in a component reference
fn collect_occurrences_in_component_ref(
    comp_ref: &ComponentReference,
    old_name: &str,
    new_name: &str,
    edits: &mut Vec<TextEdit>,
) {
    for part in &comp_ref.parts {
        if part.ident.text == old_name {
            add_edit_at_location(
                part.ident.location.start_line,
                part.ident.location.start_column,
                old_name,
                new_name,
                edits,
            );
        }
        // Check subscripts
        if let Some(subs) = &part.subs {
            for sub in subs {
                collect_occurrences_in_subscript(sub, old_name, new_name, edits);
            }
        }
    }
}

/// Collect occurrences in a subscript
fn collect_occurrences_in_subscript(
    sub: &Subscript,
    old_name: &str,
    new_name: &str,
    edits: &mut Vec<TextEdit>,
) {
    match sub {
        Subscript::Empty => {}
        Subscript::Expression(expr) => {
            collect_occurrences_in_expression(expr, old_name, new_name, edits);
        }
        Subscript::Range { .. } => {}
    }
}

/// Add a text edit at a specific location
fn add_edit_at_location(
    line: u32,
    col: u32,
    old_name: &str,
    new_name: &str,
    edits: &mut Vec<TextEdit>,
) {
    edits.push(TextEdit {
        range: Range {
            start: Position {
                line: line.saturating_sub(1),
                character: col.saturating_sub(1),
            },
            end: Position {
                line: line.saturating_sub(1),
                character: col.saturating_sub(1) + old_name.len() as u32,
            },
        },
        new_text: new_name.to_string(),
    });
}

/// Find "end ClassName" pattern and add edit
fn find_end_class_name(text: &str, old_name: &str, new_name: &str, edits: &mut Vec<TextEdit>) {
    let pattern = format!("end {}", old_name);
    for (line_num, line) in text.lines().enumerate() {
        if let Some(pos) = line.find(&pattern) {
            // Check that this is the actual end token (not part of another word)
            let after_end = pos + 4; // "end " is 4 chars
            let after_name = after_end + old_name.len();

            // Verify boundaries
            let valid_start = after_end <= line.len();
            let valid_end = after_name <= line.len();
            let char_after_valid = after_name >= line.len()
                || !line
                    .chars()
                    .nth(after_name)
                    .unwrap_or(' ')
                    .is_alphanumeric();

            if valid_start && valid_end && char_after_valid {
                edits.push(TextEdit {
                    range: Range {
                        start: Position {
                            line: line_num as u32,
                            character: after_end as u32,
                        },
                        end: Position {
                            line: line_num as u32,
                            character: after_name as u32,
                        },
                    },
                    new_text: new_name.to_string(),
                });
            }
        }
    }
}

/// Find an identifier on a specific line and add edit
fn find_identifier_on_line(
    text: &str,
    line_num: u32,
    old_name: &str,
    new_name: &str,
    edits: &mut Vec<TextEdit>,
) {
    let lines: Vec<&str> = text.lines().collect();
    if let Some(line) = lines.get((line_num.saturating_sub(1)) as usize) {
        let mut start = 0;
        while let Some(pos) = line[start..].find(old_name) {
            let abs_pos = start + pos;
            let before_valid = abs_pos == 0
                || !line
                    .chars()
                    .nth(abs_pos - 1)
                    .unwrap_or(' ')
                    .is_alphanumeric()
                    && line.chars().nth(abs_pos - 1).unwrap_or(' ') != '_';
            let after_pos = abs_pos + old_name.len();
            let after_valid = after_pos >= line.len()
                || (!line.chars().nth(after_pos).unwrap_or(' ').is_alphanumeric()
                    && line.chars().nth(after_pos).unwrap_or(' ') != '_');

            if before_valid && after_valid {
                edits.push(TextEdit {
                    range: Range {
                        start: Position {
                            line: line_num.saturating_sub(1),
                            character: abs_pos as u32,
                        },
                        end: Position {
                            line: line_num.saturating_sub(1),
                            character: after_pos as u32,
                        },
                    },
                    new_text: new_name.to_string(),
                });
            }
            start = abs_pos + 1;
        }
    }
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
            let mut edits = Vec::new();
            collect_symbol_occurrences(&doc_ast, &old_name, doc_text, &mut edits, new_name);

            if !edits.is_empty() {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_end_class_name() {
        let text = "model Test\n  Real x;\nend Test;";
        let mut edits = Vec::new();
        find_end_class_name(text, "Test", "NewName", &mut edits);
        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].range.start.line, 2);
        assert_eq!(edits[0].range.start.character, 4);
        assert_eq!(edits[0].new_text, "NewName");
    }

    #[test]
    fn test_find_identifier_on_line() {
        let text = "  Real x = y + z;";
        let mut edits = Vec::new();
        find_identifier_on_line(text, 1, "x", "newX", &mut edits);
        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].range.start.character, 7);
        assert_eq!(edits[0].new_text, "newX");
    }
}
