//! Location utilities for working with source positions.
//!
//! Provides helper functions for converting between different location representations
//! and checking position containment within source spans.

use crate::ir::ast::{Location, Token};

/// Convert a Token to an LSP-compatible range (0-indexed).
///
/// Returns (start_line, start_column, end_line, end_column) all 0-indexed.
pub fn token_to_lsp_range(token: &Token) -> (u32, u32, u32, u32) {
    let loc = &token.location;
    (
        loc.start_line.saturating_sub(1),
        loc.start_column.saturating_sub(1),
        loc.end_line.saturating_sub(1),
        loc.end_column.saturating_sub(1),
    )
}

/// Convert a Location to an LSP-compatible range (0-indexed).
///
/// Returns (start_line, start_column, end_line, end_column) all 0-indexed.
pub fn location_to_lsp_range(loc: &Location) -> (u32, u32, u32, u32) {
    (
        loc.start_line.saturating_sub(1),
        loc.start_column.saturating_sub(1),
        loc.end_line.saturating_sub(1),
        loc.end_column.saturating_sub(1),
    )
}

/// Check if a position (1-indexed) is within a location span.
///
/// Both position and location use 1-indexed line/column numbers.
pub fn position_in_location(loc: &Location, line: u32, col: u32) -> bool {
    // Check if position is within the location's start and end lines
    if line < loc.start_line || line > loc.end_line {
        return false;
    }
    // If on the start line, check column is at or after start
    if line == loc.start_line && col < loc.start_column {
        return false;
    }
    // If on the end line, check column is at or before end
    if line == loc.end_line && col > loc.end_column {
        return false;
    }
    true
}

/// Check if a position (0-indexed) is within a location span.
///
/// Position uses 0-indexed line/column, location uses 1-indexed.
/// This is a convenience method for LSP which uses 0-indexed positions.
pub fn position_in_location_0indexed(loc: &Location, line: u32, col: u32) -> bool {
    position_in_location(loc, line + 1, col + 1)
}

/// Create a location spanning from start to end token.
///
/// Useful for creating spans that cover multiple tokens.
pub fn span_tokens(start: &Token, end: &Token) -> Location {
    Location {
        start_line: start.location.start_line,
        start_column: start.location.start_column,
        end_line: end.location.end_line,
        end_column: end.location.end_column,
        start: start.location.start,
        end: end.location.end,
        file_name: start.location.file_name.clone(),
    }
}

/// Create a location spanning from start to end location.
pub fn span_locations(start: &Location, end: &Location) -> Location {
    Location {
        start_line: start.start_line,
        start_column: start.start_column,
        end_line: end.end_line,
        end_column: end.end_column,
        start: start.start,
        end: end.end,
        file_name: start.file_name.clone(),
    }
}

/// Check if one location completely contains another.
///
/// Returns true if `inner` is completely within `outer`.
pub fn location_contains(outer: &Location, inner: &Location) -> bool {
    // Check start is within or after outer start
    let start_ok = inner.start_line > outer.start_line
        || (inner.start_line == outer.start_line && inner.start_column >= outer.start_column);

    // Check end is within or before outer end
    let end_ok = inner.end_line < outer.end_line
        || (inner.end_line == outer.end_line && inner.end_column <= outer.end_column);

    start_ok && end_ok
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_token(start_line: u32, start_col: u32, end_line: u32, end_col: u32) -> Token {
        Token {
            text: String::new(),
            location: Location {
                start_line,
                start_column: start_col,
                end_line,
                end_column: end_col,
                ..Default::default()
            },
            ..Default::default()
        }
    }

    #[test]
    fn test_token_to_lsp_range() {
        let token = make_token(10, 5, 10, 15);
        let (sl, sc, el, ec) = token_to_lsp_range(&token);
        assert_eq!((sl, sc, el, ec), (9, 4, 9, 14));
    }

    #[test]
    fn test_position_in_location() {
        let loc = Location {
            start_line: 5,
            start_column: 10,
            end_line: 8,
            end_column: 20,
            ..Default::default()
        };

        // Position before start
        assert!(!position_in_location(&loc, 4, 5));
        assert!(!position_in_location(&loc, 5, 5));

        // Position within
        assert!(position_in_location(&loc, 5, 10)); // Start
        assert!(position_in_location(&loc, 6, 1)); // Middle line
        assert!(position_in_location(&loc, 8, 20)); // End

        // Position after end
        assert!(!position_in_location(&loc, 8, 21));
        assert!(!position_in_location(&loc, 9, 1));
    }

    #[test]
    fn test_span_tokens() {
        let start = make_token(5, 10, 5, 15);
        let end = make_token(10, 5, 10, 20);
        let span = span_tokens(&start, &end);

        assert_eq!(span.start_line, 5);
        assert_eq!(span.start_column, 10);
        assert_eq!(span.end_line, 10);
        assert_eq!(span.end_column, 20);
    }

    #[test]
    fn test_location_contains() {
        let outer = Location {
            start_line: 5,
            start_column: 1,
            end_line: 15,
            end_column: 50,
            ..Default::default()
        };

        let inner = Location {
            start_line: 7,
            start_column: 5,
            end_line: 10,
            end_column: 20,
            ..Default::default()
        };

        assert!(location_contains(&outer, &inner));
        assert!(!location_contains(&inner, &outer));

        // Edge case: same location
        assert!(location_contains(&outer, &outer));
    }
}
