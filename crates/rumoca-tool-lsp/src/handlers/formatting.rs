//! Document formatting handler via rumoca-tool-fmt.

use lsp_types::{Position, Range, TextEdit};

/// Handle document formatting request.
pub fn handle_formatting(source: &str) -> Option<Vec<TextEdit>> {
    let options = rumoca_tool_fmt::FormatOptions::default();
    let formatted = rumoca_tool_fmt::format(source, &options).ok()?;

    if formatted == source {
        return None;
    }

    // Return a single TextEdit replacing the entire document
    let line_count = source.lines().count() as u32;
    let last_line_len = source.lines().last().map(|l| l.len() as u32).unwrap_or(0);

    Some(vec![TextEdit {
        range: Range {
            start: Position::new(0, 0),
            end: Position::new(line_count, last_line_len),
        },
        new_text: formatted,
    }])
}
