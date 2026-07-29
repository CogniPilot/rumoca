//! Local adaptation between protocol-neutral text positions and LSP types.

use lsp_types::{Position, Range};
use rumoca_core::text_position::{self, TextPosition, TextRange};

#[must_use]
#[cfg(test)]
pub(crate) fn byte_offset_to_position(source: &str, byte_offset: usize) -> Position {
    to_lsp_position(text_position::byte_offset_to_position(source, byte_offset))
}

#[must_use]
pub(crate) fn position_to_byte_offset(source: &str, position: Position) -> usize {
    text_position::position_to_byte_offset(source, from_lsp_position(position))
}

#[must_use]
pub(crate) fn span_to_range(source: &str, start_byte: usize, end_byte: usize) -> Range {
    to_lsp_range(text_position::span_to_range(source, start_byte, end_byte))
}

const fn to_lsp_position(position: TextPosition) -> Position {
    Position {
        line: position.line,
        character: position.character,
    }
}

const fn from_lsp_position(position: Position) -> TextPosition {
    TextPosition::new(position.line, position.character)
}

const fn to_lsp_range(range: TextRange) -> Range {
    Range {
        start: to_lsp_position(range.start),
        end: to_lsp_position(range.end),
    }
}
