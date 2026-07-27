//! Document formatting handler.

use lsp_types::{Position, Range, TextEdit};
use rumoca_lsp_position::byte_offset_to_position;
use rumoca_tool_fmt::{FormatOptions, PartialFormatOptions, format};

/// Handle document formatting request.
///
/// `options` is the effective configuration for this document — see
/// [`partial_format_options_from_client`] and the server's SPEC_0018 config
/// resolution, which combine the editor's `FormattingOptions` with any
/// `.rumoca_fmt.toml` found beside the file.
pub fn handle_formatting(source: &str, options: &FormatOptions) -> Option<Vec<TextEdit>> {
    let formatted = format(source, options).ok()?;

    if formatted == source {
        return None;
    }

    // Replace the whole document. The end position must be a UTF-16 column, so
    // it is derived from the document's byte length rather than from
    // `last_line.len()` (a byte count) as an LSP column.
    Some(vec![TextEdit {
        range: Range {
            start: Position::new(0, 0),
            end: byte_offset_to_position(source, source.len()),
        },
        new_text: formatted,
    }])
}

/// Translate the editor's per-request formatting options into rumoca's partial
/// format options (SPEC_0018: editor settings are the lowest-precedence layer;
/// a `.rumoca_fmt.toml` beside the file overrides them so the CLI and the
/// editor produce identical output).
#[must_use]
pub fn partial_format_options_from_client(
    options: &lsp_types::FormattingOptions,
) -> PartialFormatOptions {
    // `tabSize`/`insertSpaces` are required by the protocol, but an editor that
    // omits them deserializes to `tab_size == 0`. A zero indent is never a real
    // preference, so treat the pair as unstated and leave the profile defaults
    // in place rather than formatting everything flush left.
    let indent_stated = options.tab_size > 0;
    PartialFormatOptions {
        indent_size: indent_stated.then_some(options.tab_size as usize),
        use_tabs: indent_stated.then_some(!options.insert_spaces),
        trim_trailing_whitespace: options.trim_trailing_whitespace,
        insert_final_newline: options.insert_final_newline,
        ..PartialFormatOptions::default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_tool_fmt::FormatProfile;

    #[test]
    fn replacement_range_end_uses_utf16_columns() {
        // The final line holds a two-UTF-16-unit astral character inside a
        // string, so its byte length (7) differs from its UTF-16 length (5).
        let source = "model M\n      Real x=1;\nend M;\n// 𝔸\n";
        let options = FormatOptions::for_profile(FormatProfile::Canonical);
        let edits = handle_formatting(source, &options).expect("formatting should change source");
        assert_eq!(
            edits[0].range.end,
            byte_offset_to_position(source, source.len())
        );
    }

    #[test]
    fn client_options_map_onto_partial_format_options() {
        let client = lsp_types::FormattingOptions {
            tab_size: 4,
            insert_spaces: true,
            trim_trailing_whitespace: Some(false),
            insert_final_newline: Some(true),
            ..lsp_types::FormattingOptions::default()
        };
        let partial = partial_format_options_from_client(&client);
        assert_eq!(partial.indent_size, Some(4));
        assert_eq!(partial.use_tabs, Some(false));
        assert_eq!(partial.trim_trailing_whitespace, Some(false));
        assert_eq!(partial.insert_final_newline, Some(true));
        assert_eq!(partial.profile, None, "client cannot pick a rumoca profile");
    }

    #[test]
    fn unstated_client_indent_leaves_profile_defaults_alone() {
        let partial = partial_format_options_from_client(&lsp_types::FormattingOptions::default());
        assert_eq!(
            partial.indent_size, None,
            "a zero tab_size is not a real preference"
        );
        assert_eq!(partial.use_tabs, None);
    }
}
