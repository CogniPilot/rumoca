//! Workspace symbols handler for Modelica files.

use std::path::Path;

use lsp_types::{Location, Position, Range, SymbolInformation, SymbolKind, Url};
use rumoca_session::{compile::WorkspaceSymbol, compile::WorkspaceSymbolKind, parsing::ast};

use crate::helpers::location_to_range;

/// Handle workspace symbols request - fuzzy search across all documents.
pub fn handle_workspace_symbols(entries: &[WorkspaceSymbol]) -> Vec<SymbolInformation> {
    let mut symbols = Vec::with_capacity(entries.len());

    for symbol in entries {
        let Some(uri) = workspace_symbol_uri(&symbol.uri) else {
            continue;
        };
        let kind = match_symbol_kind(&symbol.kind);
        let range = location_to_range(&symbol.location);
        symbols.push(new_symbol_information(
            symbol.name.clone(),
            kind,
            Location { uri, range },
            symbol.container_name.clone(),
        ));
    }

    symbols
}

fn workspace_symbol_uri(uri: &str) -> Option<Url> {
    if uri.contains("://") {
        return Url::parse(uri).ok();
    }
    url_from_file_path(uri)
}

#[cfg(not(target_arch = "wasm32"))]
fn url_from_file_path(path: impl AsRef<Path>) -> Option<Url> {
    Url::from_file_path(path).ok()
}

#[cfg(target_arch = "wasm32")]
fn url_from_file_path(path: impl AsRef<Path>) -> Option<Url> {
    let raw = path.as_ref().to_string_lossy();
    if raw.is_empty() {
        return None;
    }
    let mut normalized = raw.replace('\\', "/");
    if !normalized.starts_with('/') {
        normalized.insert(0, '/');
    }
    Url::parse(&format!("file://{}", normalized)).ok()
}

fn match_symbol_kind(kind: &WorkspaceSymbolKind) -> SymbolKind {
    match kind {
        WorkspaceSymbolKind::Class(class_type) => match class_type {
            ast::ClassType::Model | ast::ClassType::Block | ast::ClassType::Class => {
                SymbolKind::CLASS
            }
            ast::ClassType::Connector => SymbolKind::INTERFACE,
            ast::ClassType::Record => SymbolKind::STRUCT,
            ast::ClassType::Type => SymbolKind::TYPE_PARAMETER,
            ast::ClassType::Package => SymbolKind::NAMESPACE,
            ast::ClassType::Function => SymbolKind::FUNCTION,
            ast::ClassType::Operator => SymbolKind::OPERATOR,
        },
        WorkspaceSymbolKind::Component => SymbolKind::VARIABLE,
    }
}

#[expect(
    deprecated,
    reason = "lsp-types still requires deprecated field; remove once lsp-types drops it"
)]
fn new_symbol_information(
    name: String,
    kind: SymbolKind,
    location: Location,
    container_name: Option<String>,
) -> SymbolInformation {
    SymbolInformation {
        name,
        kind,
        tags: None,
        deprecated: None,
        location,
        container_name,
    }
}

/// Collect all class names and their ranges for code lens / diagnostics.
pub fn collect_model_names(ast: &ast::StoredDefinition) -> Vec<(String, Range)> {
    let mut names = Vec::new();
    for (name, class) in &ast.classes {
        if matches!(
            class.class_type,
            ast::ClassType::Model | ast::ClassType::Block | ast::ClassType::Class
        ) {
            let range = Range {
                start: Position::new(
                    class.name.location.start_line.saturating_sub(1),
                    class.name.location.start_column.saturating_sub(1),
                ),
                end: Position::new(
                    class.name.location.end_line.saturating_sub(1),
                    class.name.location.end_column.saturating_sub(1),
                ),
            };
            names.push((name.clone(), range));
        }
    }
    names
}
