//! Document symbols handler for Modelica files (file outline).
//!
//! Consumes query-ready symbol data produced by `rumoca-compile`.

use lsp_types::{DocumentSymbol, DocumentSymbolResponse, SymbolKind};
use rumoca_compile::compile::{DocumentSymbol as QueryDocumentSymbol, DocumentSymbolKind};

use crate::helpers::location_to_range_in_source;

/// Handle document symbols request - provides file outline.
///
/// `source` is the text the symbols were produced from; it is required to
/// convert byte spans / lexer character columns into UTF-16 LSP columns.
pub fn handle_document_symbols(
    symbols: Vec<QueryDocumentSymbol>,
    source: &str,
) -> Option<DocumentSymbolResponse> {
    let symbols = symbols
        .iter()
        .map(|symbol| to_lsp_symbol(symbol, source))
        .collect::<Vec<_>>();
    Some(DocumentSymbolResponse::Nested(symbols))
}

fn to_lsp_symbol(symbol: &QueryDocumentSymbol, source: &str) -> DocumentSymbol {
    let range = location_to_range_in_source(source, &symbol.range);
    let selection_range = clamp_selection_range(
        range,
        location_to_range_in_source(source, &symbol.selection_range),
    );
    let children = symbol
        .children
        .iter()
        .map(|child| to_lsp_symbol(child, source))
        .collect::<Vec<_>>();
    #[expect(
        deprecated,
        reason = "lsp-types still requires deprecated field; remove once the field is dropped"
    )]
    DocumentSymbol {
        name: symbol.name.clone(),
        detail: symbol.detail.clone(),
        kind: match &symbol.kind {
            DocumentSymbolKind::Class(ct) => class_type_to_symbol_kind(ct),
            DocumentSymbolKind::ParametersSection
            | DocumentSymbolKind::InputsSection
            | DocumentSymbolKind::OutputsSection
            | DocumentSymbolKind::VariablesSection
            | DocumentSymbolKind::EquationsSection
            | DocumentSymbolKind::AlgorithmsSection
            | DocumentSymbolKind::Component => SymbolKind::NAMESPACE,
        },
        tags: None,
        deprecated: None,
        range,
        selection_range,
        children: if children.is_empty() {
            None
        } else {
            Some(children)
        },
    }
}

fn clamp_selection_range(
    range: lsp_types::Range,
    selection_range: lsp_types::Range,
) -> lsp_types::Range {
    if range_contains(range, selection_range) {
        selection_range
    } else {
        range
    }
}

fn range_contains(outer: lsp_types::Range, inner: lsp_types::Range) -> bool {
    position_leq(outer.start, inner.start) && position_leq(inner.end, outer.end)
}

fn position_leq(left: lsp_types::Position, right: lsp_types::Position) -> bool {
    left.line < right.line || (left.line == right.line && left.character <= right.character)
}

fn class_type_to_symbol_kind(ct: &rumoca_compile::parsing::ir_core::ClassType) -> SymbolKind {
    match ct {
        rumoca_compile::parsing::ir_core::ClassType::Model
        | rumoca_compile::parsing::ir_core::ClassType::Block
        | rumoca_compile::parsing::ir_core::ClassType::Class => SymbolKind::CLASS,
        rumoca_compile::parsing::ir_core::ClassType::Connector => SymbolKind::INTERFACE,
        rumoca_compile::parsing::ir_core::ClassType::Record => SymbolKind::STRUCT,
        rumoca_compile::parsing::ir_core::ClassType::Type => SymbolKind::TYPE_PARAMETER,
        rumoca_compile::parsing::ir_core::ClassType::Package => SymbolKind::NAMESPACE,
        rumoca_compile::parsing::ir_core::ClassType::Function => SymbolKind::FUNCTION,
        rumoca_compile::parsing::ir_core::ClassType::Operator => SymbolKind::OPERATOR,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::Position;

    /// Four ASCII lines so the char-column fallback has real line text to
    /// measure against (line index 1 is `model M`, seven characters wide).
    const ASCII_SOURCE: &str = "model M\nmodel M\n  Real x;\nend M;\n";

    #[test]
    fn invalid_selection_range_falls_back_to_symbol_range() {
        let symbol = QueryDocumentSymbol {
            name: "M".to_string(),
            detail: None,
            kind: DocumentSymbolKind::Class(rumoca_compile::parsing::ir_core::ClassType::Model),
            range: rumoca_compile::parsing::ir_core::Location {
                start_line: 2,
                start_column: 1,
                end_line: 4,
                end_column: 10,
                ..Default::default()
            },
            selection_range: rumoca_compile::parsing::ir_core::Location {
                start_line: 1,
                start_column: 1,
                end_line: 1,
                end_column: 5,
                ..Default::default()
            },
            children: Vec::new(),
        };

        let lsp_symbol = to_lsp_symbol(&symbol, ASCII_SOURCE);
        assert_eq!(lsp_symbol.selection_range, lsp_symbol.range);
    }

    #[test]
    fn valid_selection_range_is_preserved() {
        let symbol = QueryDocumentSymbol {
            name: "M".to_string(),
            detail: None,
            kind: DocumentSymbolKind::Class(rumoca_compile::parsing::ir_core::ClassType::Model),
            range: rumoca_compile::parsing::ir_core::Location {
                start_line: 2,
                start_column: 1,
                end_line: 4,
                end_column: 10,
                ..Default::default()
            },
            selection_range: rumoca_compile::parsing::ir_core::Location {
                start_line: 2,
                start_column: 7,
                end_line: 2,
                end_column: 8,
                ..Default::default()
            },
            children: Vec::new(),
        };

        let lsp_symbol = to_lsp_symbol(&symbol, ASCII_SOURCE);
        assert_eq!(
            lsp_symbol.selection_range.start,
            Position {
                line: 1,
                character: 6,
            }
        );
        assert_eq!(
            lsp_symbol.selection_range.end,
            Position {
                line: 1,
                character: 7,
            }
        );
    }

    #[test]
    fn byte_spans_win_over_lexer_columns_for_non_ascii_lines() {
        // `𝔸` is one lexer column but two UTF-16 units; the byte span is the
        // authoritative form and must be used when present.
        let source = "model M\n  Real 𝔸x = 1;\nend M;\n";
        let name_start = source.find("𝔸x").expect("component name present");
        let name_end = name_start + "𝔸x".len();
        let symbol = QueryDocumentSymbol {
            name: "𝔸x".to_string(),
            detail: None,
            kind: DocumentSymbolKind::Component,
            range: rumoca_compile::parsing::ir_core::Location {
                start_line: 2,
                start_column: 8,
                end_line: 2,
                end_column: 10,
                start: name_start as u32,
                end: name_end as u32,
                ..Default::default()
            },
            selection_range: rumoca_compile::parsing::ir_core::Location {
                start_line: 2,
                start_column: 8,
                end_line: 2,
                end_column: 10,
                start: name_start as u32,
                end: name_end as u32,
                ..Default::default()
            },
            children: Vec::new(),
        };

        let lsp_symbol = to_lsp_symbol(&symbol, source);
        // `  Real ` is 7 UTF-16 units, so `𝔸` starts at column 7 and the whole
        // `𝔸x` name ends at column 10 (2 units for the astral char + 1).
        assert_eq!(lsp_symbol.range.start, Position::new(1, 7));
        assert_eq!(lsp_symbol.range.end, Position::new(1, 10));
    }

    #[test]
    fn char_column_fallback_counts_utf16_units() {
        // A synthesized location (no byte span) still has to report UTF-16
        // columns, not raw lexer character columns.
        let source = "model M\n  Real 𝔸x = 1;\nend M;\n";
        let symbol = QueryDocumentSymbol {
            name: "Variables".to_string(),
            detail: None,
            kind: DocumentSymbolKind::VariablesSection,
            range: rumoca_compile::parsing::ir_core::Location {
                start_line: 2,
                start_column: 8,
                end_line: 2,
                end_column: 10,
                ..Default::default()
            },
            selection_range: rumoca_compile::parsing::ir_core::Location {
                start_line: 2,
                start_column: 8,
                end_line: 2,
                end_column: 10,
                ..Default::default()
            },
            children: Vec::new(),
        };

        let lsp_symbol = to_lsp_symbol(&symbol, source);
        assert_eq!(lsp_symbol.range.start, Position::new(1, 7));
        assert_eq!(lsp_symbol.range.end, Position::new(1, 10));
    }
}
