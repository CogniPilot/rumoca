//! Document symbols handler for Modelica files (file outline).
//!
//! Consumes query-ready symbol data produced by `rumoca-session`.

use lsp_types::{DocumentSymbol, DocumentSymbolResponse, SymbolKind};
use rumoca_session::compile::{DocumentSymbol as QueryDocumentSymbol, DocumentSymbolKind};
use rumoca_session::parsing::ast;

use crate::helpers::location_to_range;

/// Handle document symbols request - provides file outline.
pub fn handle_document_symbols(
    symbols: Vec<QueryDocumentSymbol>,
) -> Option<DocumentSymbolResponse> {
    let symbols = symbols.iter().map(to_lsp_symbol).collect::<Vec<_>>();
    Some(DocumentSymbolResponse::Nested(symbols))
}

fn to_lsp_symbol(symbol: &QueryDocumentSymbol) -> DocumentSymbol {
    let range = location_to_range(&symbol.range);
    let selection_range = clamp_selection_range(range, location_to_range(&symbol.selection_range));
    let children = symbol
        .children
        .iter()
        .map(to_lsp_symbol)
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

fn class_type_to_symbol_kind(ct: &ast::ClassType) -> SymbolKind {
    match ct {
        ast::ClassType::Model | ast::ClassType::Block | ast::ClassType::Class => SymbolKind::CLASS,
        ast::ClassType::Connector => SymbolKind::INTERFACE,
        ast::ClassType::Record => SymbolKind::STRUCT,
        ast::ClassType::Type => SymbolKind::TYPE_PARAMETER,
        ast::ClassType::Package => SymbolKind::NAMESPACE,
        ast::ClassType::Function => SymbolKind::FUNCTION,
        ast::ClassType::Operator => SymbolKind::OPERATOR,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::Position;

    #[test]
    fn invalid_selection_range_falls_back_to_symbol_range() {
        let symbol = QueryDocumentSymbol {
            name: "M".to_string(),
            detail: None,
            kind: DocumentSymbolKind::Class(ast::ClassType::Model),
            range: ast::Location {
                start_line: 2,
                start_column: 1,
                end_line: 4,
                end_column: 10,
                ..Default::default()
            },
            selection_range: ast::Location {
                start_line: 1,
                start_column: 1,
                end_line: 1,
                end_column: 5,
                ..Default::default()
            },
            children: Vec::new(),
        };

        let lsp_symbol = to_lsp_symbol(&symbol);
        assert_eq!(lsp_symbol.selection_range, lsp_symbol.range);
    }

    #[test]
    fn valid_selection_range_is_preserved() {
        let symbol = QueryDocumentSymbol {
            name: "M".to_string(),
            detail: None,
            kind: DocumentSymbolKind::Class(ast::ClassType::Model),
            range: ast::Location {
                start_line: 2,
                start_column: 1,
                end_line: 4,
                end_column: 10,
                ..Default::default()
            },
            selection_range: ast::Location {
                start_line: 2,
                start_column: 7,
                end_line: 2,
                end_column: 8,
                ..Default::default()
            },
            children: Vec::new(),
        };

        let lsp_symbol = to_lsp_symbol(&symbol);
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
}
