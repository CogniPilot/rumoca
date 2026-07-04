//! Byte-offset → LSP [`Position`] conversion (UTF-16), WASM-safe.
//!
//! GALEC spans (`rumoca_core::Span`) and parse-error offsets are UTF-8 **byte**
//! offsets; LSP `Position.character` counts **UTF-16 code units** (the default
//! `positionEncoding`). This mirrors the Modelica LSP's converter
//! (`rumoca-tool-lsp/src/handlers/diagnostics.rs`) — the single, shared byte↔
//! UTF-16 boundary — rather than introducing a fourth coordinate system.

use lsp_types::{Position, Range};

/// Convert a UTF-8 byte offset into an LSP [`Position`] (line, UTF-16 column).
/// Clamps out-of-range offsets to the end of `source`; `\n` starts a new line.
#[must_use]
pub fn byte_offset_to_position(source: &str, byte_offset: usize) -> Position {
    let clamped = byte_offset.min(source.len());
    let mut line = 0u32;
    let mut col_utf16 = 0u32;

    for (idx, ch) in source.char_indices() {
        if idx >= clamped {
            break;
        }
        if ch == '\n' {
            line = line.saturating_add(1);
            col_utf16 = 0;
        } else {
            col_utf16 = col_utf16.saturating_add(ch.len_utf16() as u32);
        }
    }

    Position::new(line, col_utf16)
}

/// Convert a `[start, end)` byte range into an LSP [`Range`]. A zero-width or
/// inverted range is widened to one column so editors still render a mark.
#[must_use]
pub fn span_to_range(source: &str, start_byte: usize, end_byte: usize) -> Range {
    let start = byte_offset_to_position(source, start_byte);
    let mut end = byte_offset_to_position(source, end_byte);
    if (end.line < start.line) || (end.line == start.line && end.character <= start.character) {
        end = Position::new(start.line, start.character.saturating_add(1));
    }
    Range { start, end }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn byte_offset_maps_lines_and_utf16_columns() {
        let source = "block A\n  x := 1;\n";
        // Offset 0 -> line 0, col 0.
        assert_eq!(byte_offset_to_position(source, 0), Position::new(0, 0));
        // The `x` after the newline + two spaces -> line 1, col 2.
        let x = source.find('x').unwrap();
        assert_eq!(byte_offset_to_position(source, x), Position::new(1, 2));
    }

    #[test]
    fn columns_count_utf16_code_units_not_bytes() {
        // `é` is 2 UTF-8 bytes but 1 UTF-16 code unit; an astral char is 4 bytes
        // / 2 UTF-16 units. Column arithmetic must be UTF-16, not byte-based.
        let source = "é𝔸x";
        let x = source.find('x').unwrap(); // byte offset 6
        // 1 (é) + 2 (𝔸 surrogate pair) = 3 UTF-16 units before `x`.
        assert_eq!(byte_offset_to_position(source, x), Position::new(0, 3));
    }

    #[test]
    fn zero_width_range_is_widened() {
        let source = "abc";
        let range = span_to_range(source, 1, 1);
        assert_eq!(range.start, Position::new(0, 1));
        assert_eq!(range.end, Position::new(0, 2));
    }

    #[test]
    fn out_of_range_offset_clamps_to_end() {
        let source = "ab";
        assert_eq!(byte_offset_to_position(source, 999), Position::new(0, 2));
    }
}
