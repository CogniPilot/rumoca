//! GALEC hover + go-to-definition, WASM-safe (lsp-types only).
//!
//! Both parse the document and locate the symbol under the cursor via
//! [`rumoca_ir_galec::symbol_at`], which reuses the validator's name resolution
//! and the source spans on references and declarations. A document that does
//! not parse yields no navigation (there is no AST to walk).

use lsp_types::{
    GotoDefinitionResponse, Hover, HoverContents, Location, MarkupContent, MarkupKind, Position,
    Url,
};

use rumoca_ir_galec::parse::parse;
use rumoca_ir_galec::symbol_at;

use crate::position::{position_to_byte_offset, span_to_range};

/// Hover summary for the symbol at `position`, or `None` when the cursor is not
/// on a resolvable reference (or the document does not parse).
#[must_use]
pub fn hover(source: &str, file_name: &str, position: Position) -> Option<Hover> {
    let block = parse(source, file_name).ok()?;
    let offset = position_to_byte_offset(source, position);
    let info = symbol_at(&block, offset)?;
    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: format!("```galec\n{}\n```", info.hover),
        }),
        range: Some(span_to_range(
            source,
            info.reference_span.start.0,
            info.reference_span.end.0,
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
    let block = parse(source, file_name).ok()?;
    let offset = position_to_byte_offset(source, position);
    let definition = symbol_at(&block, offset)?.definition_span?;
    let location = Location {
        uri,
        range: span_to_range(source, definition.start.0, definition.end.0),
    };
    Some(GotoDefinitionResponse::Scalar(location))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_ir_galec::ast::{
        Block, Expression, InterfaceKind, InterfaceVariable, Name, Reference, ScalarType, Spanned,
        Statement, VariableDeclaration,
    };

    /// A block whose `DoStep` assigns `self.y := self.u`, printed to text.
    fn sample_source() -> String {
        let mut block = Block::new(Name::ident("Nav"));
        let interface = |kind, name: &str| InterfaceVariable {
            kind,
            decl: VariableDeclaration::scalar(ScalarType::Real, Name::ident(name)),
            start: None,
        };
        block.interface.push(interface(InterfaceKind::Input, "u"));
        block.interface.push(interface(InterfaceKind::Output, "y"));
        block
            .do_step
            .statements
            .push(Spanned::dummy(Statement::Assignment {
                target: Reference::state(Name::ident("y")),
                value: Expression::Ref(Reference::state(Name::ident("u"))),
            }));
        rumoca_ir_galec::print_block(&block).expect("prints")
    }

    /// The line/column of the `u` in the `self.u` reference.
    fn reference_position(source: &str) -> Position {
        let offset = source.find("self.u").expect("self.u present") + "self.".len();
        crate::position::byte_offset_to_position(source, offset)
    }

    #[test]
    fn hover_shows_the_declared_type() {
        let source = sample_source();
        let hover = hover(&source, "nav.alg", reference_position(&source)).expect("hover present");
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
        let response = goto_definition(&source, "nav.alg", uri, reference_position(&source))
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
        assert!(hover(&source, "nav.alg", position).is_none());
    }
}
