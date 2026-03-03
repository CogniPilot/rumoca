//! Rename handler for Modelica files.

use lsp_types::{Position, PrepareRenameResponse, TextEdit, Url, WorkspaceEdit};
use rumoca_ir_ast as ast;

use crate::helpers::get_word_at_position;

use super::occurrence::find_all_occurrences;

/// Handle prepare-rename request - validate that cursor is on a renameable symbol.
pub fn handle_prepare_rename(
    source: &str,
    line: u32,
    character: u32,
) -> Option<PrepareRenameResponse> {
    let position = Position { line, character };
    let word = get_word_at_position(source, position)?;

    // Don't allow renaming keywords or builtins
    if is_keyword(&word) || rumoca_core::is_builtin_function(&word) {
        return None;
    }

    // Return the range of the word at cursor
    let lines: Vec<&str> = source.lines().collect();
    let line_text = lines.get(line as usize)?;
    let col = character as usize;
    let start = line_text[..col]
        .rfind(|c: char| !c.is_alphanumeric() && c != '_')
        .map(|i| i + 1)
        .unwrap_or(0);
    let end = line_text[col..]
        .find(|c: char| !c.is_alphanumeric() && c != '_')
        .map(|i| col + i)
        .unwrap_or(line_text.len());

    let range = lsp_types::Range {
        start: Position::new(line, start as u32),
        end: Position::new(line, end as u32),
    };

    Some(PrepareRenameResponse::Range(range))
}

/// Handle rename request - rename all occurrences.
pub fn handle_rename(
    ast: &ast::StoredDefinition,
    source: &str,
    uri: &Url,
    line: u32,
    character: u32,
    new_name: &str,
) -> Option<WorkspaceEdit> {
    let position = Position { line, character };
    let word = get_word_at_position(source, position)?;

    if is_keyword(&word) || rumoca_core::is_builtin_function(&word) {
        return None;
    }

    let occurrences = find_all_occurrences(ast, &word, true);
    if occurrences.is_empty() {
        return None;
    }

    let edits: Vec<TextEdit> = occurrences
        .into_iter()
        .map(|occ| TextEdit {
            range: occ.range,
            new_text: new_name.to_string(),
        })
        .collect();

    let mut changes = std::collections::HashMap::new();
    changes.insert(uri.clone(), edits);

    Some(WorkspaceEdit {
        changes: Some(changes),
        document_changes: None,
        change_annotations: None,
    })
}

fn is_keyword(word: &str) -> bool {
    matches!(
        word,
        "model"
            | "package"
            | "function"
            | "block"
            | "connector"
            | "record"
            | "type"
            | "class"
            | "equation"
            | "algorithm"
            | "parameter"
            | "constant"
            | "input"
            | "output"
            | "extends"
            | "import"
            | "if"
            | "then"
            | "else"
            | "elseif"
            | "for"
            | "when"
            | "while"
            | "end"
            | "der"
            | "connect"
            | "Real"
            | "Integer"
            | "Boolean"
            | "String"
            | "true"
            | "false"
            | "each"
            | "final"
            | "inner"
            | "outer"
            | "flow"
            | "stream"
            | "replaceable"
            | "redeclare"
            | "encapsulated"
            | "partial"
            | "not"
            | "and"
            | "or"
            | "in"
            | "loop"
    )
}
