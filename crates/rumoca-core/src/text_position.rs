//! Protocol-neutral UTF-8 byte offset ↔ UTF-16 text position conversion.
//!
//! Compiler spans use UTF-8 byte offsets, while editor protocols commonly use
//! zero-based lines and UTF-16 code-unit columns. The compact types in this
//! module keep that conversion independent of any transport protocol.

/// A zero-based line and UTF-16 code-unit column.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct TextPosition {
    /// Zero-based line number.
    pub line: u32,
    /// Zero-based UTF-16 code-unit column.
    pub character: u32,
}

impl TextPosition {
    /// Construct a text position from a zero-based line and UTF-16 column.
    #[must_use]
    pub const fn new(line: u32, character: u32) -> Self {
        Self { line, character }
    }
}

/// A half-open range between two text positions.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct TextRange {
    /// Inclusive start position.
    pub start: TextPosition,
    /// Exclusive end position.
    pub end: TextPosition,
}

impl TextRange {
    /// Construct a half-open text range.
    #[must_use]
    pub const fn new(start: TextPosition, end: TextPosition) -> Self {
        Self { start, end }
    }
}

/// Convert a UTF-8 byte offset into a zero-based line and UTF-16 column.
///
/// Out-of-range offsets clamp to the end of `source`; `\n` starts a new line.
#[must_use]
pub fn byte_offset_to_position(source: &str, byte_offset: usize) -> TextPosition {
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

    TextPosition::new(line, col_utf16)
}

/// Convert a zero-based line and UTF-16 column into a UTF-8 byte offset.
///
/// Positions past a line or the document clamp forward to the next valid byte
/// boundary. A column in the middle of a UTF-16 surrogate pair clamps forward
/// to the end of that Unicode scalar.
#[must_use]
pub fn position_to_byte_offset(source: &str, position: TextPosition) -> usize {
    let mut line = 0u32;
    let mut col_utf16 = 0u32;
    for (idx, ch) in source.char_indices() {
        if line > position.line || (line == position.line && col_utf16 >= position.character) {
            return idx;
        }
        if ch == '\n' && line == position.line {
            return idx;
        }
        if ch == '\n' {
            line = line.saturating_add(1);
            col_utf16 = 0;
        } else {
            col_utf16 = col_utf16.saturating_add(ch.len_utf16() as u32);
        }
    }
    source.len()
}

/// Convert a `[start, end)` UTF-8 byte range into a UTF-16 text range.
///
/// A zero-width or inverted range is widened to one UTF-16 column so editor
/// clients can display it.
#[must_use]
pub fn span_to_range(source: &str, start_byte: usize, end_byte: usize) -> TextRange {
    let start = byte_offset_to_position(source, start_byte);
    let mut end = byte_offset_to_position(source, end_byte);
    if (end.line < start.line) || (end.line == start.line && end.character <= start.character) {
        end = TextPosition::new(start.line, start.character.saturating_add(1));
    }
    TextRange::new(start, end)
}

/// Return a zero-based line without a trailing `\r`.
///
/// Stripping `\r` makes CRLF documents behave like LF documents.
#[must_use]
pub fn line_text(source: &str, line: u32) -> Option<&str> {
    let raw = source.split('\n').nth(line as usize)?;
    Some(raw.strip_suffix('\r').unwrap_or(raw))
}

/// Convert a UTF-16 column within one line to a UTF-8 byte column.
///
/// The result is always a valid character boundary. Columns past the end clamp
/// to `line.len()`.
#[must_use]
pub fn utf16_column_to_byte_column(line: &str, character: u32) -> usize {
    position_to_byte_offset(line, TextPosition::new(0, character)).min(line.len())
}

/// Convert a 1-based Unicode-scalar column to a zero-based UTF-16 column.
///
/// This supports lexer locations that count one column per Unicode scalar.
/// Columns past the end clamp to the line's UTF-16 length.
#[must_use]
pub fn char_column_to_utf16_column(line: &str, char_column_1based: u32) -> u32 {
    let chars_before = char_column_1based.saturating_sub(1) as usize;
    line.chars()
        .take(chars_before)
        .fold(0u32, |acc, ch| acc.saturating_add(ch.len_utf16() as u32))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn positions_and_ranges_are_compact() {
        assert_eq!(std::mem::size_of::<TextPosition>(), 8);
        assert_eq!(std::mem::size_of::<TextRange>(), 16);
    }

    #[test]
    fn byte_offset_maps_lines_and_utf16_columns() {
        let source = "block A\n  x := 1;\n";
        assert_eq!(byte_offset_to_position(source, 0), TextPosition::new(0, 0));
        let x = source.find('x').unwrap();
        assert_eq!(byte_offset_to_position(source, x), TextPosition::new(1, 2));
    }

    #[test]
    fn columns_count_utf16_code_units_not_bytes() {
        let source = "é𝔸x";
        let x = source.find('x').unwrap();
        assert_eq!(byte_offset_to_position(source, x), TextPosition::new(0, 3));
    }

    #[test]
    fn zero_width_range_is_widened() {
        let range = span_to_range("abc", 1, 1);
        assert_eq!(range.start, TextPosition::new(0, 1));
        assert_eq!(range.end, TextPosition::new(0, 2));
    }

    #[test]
    fn out_of_range_offset_clamps_to_end() {
        assert_eq!(byte_offset_to_position("ab", 999), TextPosition::new(0, 2));
    }

    #[test]
    fn position_past_end_of_line_clamps_to_line_end() {
        let source = "ab\ncd\n";
        assert_eq!(position_to_byte_offset(source, TextPosition::new(0, 99)), 2);
        assert_eq!(position_to_byte_offset(source, TextPosition::new(0, 2)), 2);
    }

    #[test]
    fn position_round_trips_through_byte_offset() {
        let source = "block A\n  x := 𝔸 é;\n";
        for (offset, _) in source.char_indices() {
            let position = byte_offset_to_position(source, offset);
            assert_eq!(
                position_to_byte_offset(source, position),
                offset,
                "round-trip failed at byte {offset}"
            );
        }
    }

    #[test]
    fn position_to_byte_offset_handles_astral_surrogate_pairs() {
        let source = "𝔸x";
        assert_eq!(position_to_byte_offset(source, TextPosition::new(0, 0)), 0);
        assert_eq!(position_to_byte_offset(source, TextPosition::new(0, 2)), 4);
        assert_eq!(position_to_byte_offset(source, TextPosition::new(0, 1)), 4);
        assert_eq!(position_to_byte_offset(source, TextPosition::new(0, 3)), 5);
    }

    #[test]
    fn char_column_to_utf16_column_counts_surrogate_pairs() {
        let line = "𝔸x";
        assert_eq!(char_column_to_utf16_column(line, 1), 0);
        assert_eq!(char_column_to_utf16_column(line, 2), 2);
        assert_eq!(char_column_to_utf16_column(line, 99), 3);
        assert_eq!(char_column_to_utf16_column(line, 0), 0);
    }

    #[test]
    fn utf16_column_to_byte_column_clamps_forward() {
        let line = "𝔸x";
        assert_eq!(utf16_column_to_byte_column(line, 1), 4);
        assert_eq!(utf16_column_to_byte_column(line, 2), 4);
        assert_eq!(utf16_column_to_byte_column(line, 99), line.len());
        for character in 0..8u32 {
            let byte = utf16_column_to_byte_column(line, character);
            assert!(line.is_char_boundary(byte), "byte {byte} is not a boundary");
        }
    }

    #[test]
    fn line_text_strips_crlf() {
        let source = "alpha\r\nbeta\r\n";
        assert_eq!(line_text(source, 0), Some("alpha"));
        assert_eq!(line_text(source, 1), Some("beta"));
        assert_eq!(line_text(source, 3), None);
    }
}
