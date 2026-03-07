//! Find-all-references handler for Modelica files.

use lsp_types::{Location, Position, Url};
use rumoca_session::parsing::ast;

use crate::helpers::get_word_at_position;

use super::occurrence::find_all_occurrences;

/// Handle find-references request.
pub fn handle_references(
    ast: &ast::StoredDefinition,
    source: &str,
    uri: &Url,
    line: u32,
    character: u32,
    include_declaration: bool,
) -> Option<Vec<Location>> {
    let position = Position { line, character };
    let word = get_word_at_position(source, position)?;

    let occurrences = find_all_occurrences(ast, &word, include_declaration);
    if occurrences.is_empty() {
        return None;
    }

    let locations = occurrences
        .into_iter()
        .map(|occ| Location {
            uri: uri.clone(),
            range: occ.range,
        })
        .collect();

    Some(locations)
}
