//! GALEC hover + go-to-definition, WASM-safe (lsp-types only).
//!
//! Both parse the document and locate the symbol under the cursor via
//! the parse phase's opaque document query, which reuses checked name
//! resolution and source spans without exposing an unchecked block. A
//! document that does not parse yields no navigation.

use lsp_types::{
    GotoDefinitionResponse, Hover, HoverContents, Location, MarkupContent, MarkupKind, Position,
    Url,
};

use rumoca_phase_parse_galec::parse_document;

use crate::text_position::{position_to_byte_offset, span_to_range};

/// Hover summary for the symbol at `position`, or `None` when the cursor is not
/// on a resolvable reference (or the document does not parse).
#[must_use]
pub fn hover(source: &str, file_name: &str, position: Position) -> Option<Hover> {
    let document = parse_document(source, file_name).ok()?;
    let offset = position_to_byte_offset(source, position);
    let info = document.symbol_at(offset)?;
    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: format!("```galec\n{}\n```", info.hover()),
        }),
        range: Some(span_to_range(
            source,
            info.reference_span().start.0,
            info.reference_span().end.0,
        )),
    })
}

/// The declaration location for the symbol at `position`, or `None` when the
/// cursor is not on a reference with a source declaration (e.g. a builtin) or
/// the document does not parse.
#[must_use]
pub fn goto_definition(
    source: &str,
    file_name: &str,
    uri: Url,
    position: Position,
) -> Option<GotoDefinitionResponse> {
    let document = parse_document(source, file_name).ok()?;
    let offset = position_to_byte_offset(source, position);
    let definition = document.symbol_at(offset)?.definition_span()?;
    let location = Location {
        uri,
        range: span_to_range(source, definition.start.0, definition.end.0),
    };
    Some(GotoDefinitionResponse::Scalar(location))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::text_position::byte_offset_to_position;

    fn sample_source() -> &'static str {
        "block Nav
input Real u;
output Real y;
protected
public
method Startup
algorithm
end Startup;
method Recalibrate
algorithm
end Recalibrate;
method DoStep
algorithm
self.y := self.u;
end DoStep;
end Nav;
"
    }

    /// The line/column of the `u` in the `self.u` reference.
    fn reference_position(source: &str) -> Position {
        let offset = source.find("self.u").expect("self.u present") + "self.".len();
        byte_offset_to_position(source, offset)
    }

    #[test]
    fn hover_shows_the_declared_type() {
        let source = sample_source();
        let hover = hover(source, "nav.alg", reference_position(source)).expect("hover present");
        let HoverContents::Markup(markup) = hover.contents else {
            panic!("expected markdown hover");
        };
        assert!(
            markup.value.contains("Real"),
            "hover shows type: {}",
            markup.value
        );
        assert!(hover.range.is_some(), "hover has a range");
    }

    #[test]
    fn goto_definition_jumps_to_the_declaration() {
        let source = sample_source();
        let uri = Url::parse("file:///nav.alg").unwrap();
        let response = goto_definition(source, "nav.alg", uri, reference_position(source))
            .expect("definition present");
        let GotoDefinitionResponse::Scalar(location) = response else {
            panic!("expected a single definition location");
        };
        // The definition range slices the `u` declaration name.
        let line = source
            .lines()
            .nth(location.range.start.line as usize)
            .unwrap();
        let start = location.range.start.character as usize;
        let end = location.range.end.character as usize;
        assert_eq!(&line[start..end], "u");
    }

    #[test]
    fn no_navigation_off_a_reference() {
        let source = sample_source();
        // Column 0 of the first line (`block Nav`) is a keyword, not a reference.
        let position = Position::new(0, 0);
        assert!(hover(source, "nav.alg", position).is_none());
    }
}
