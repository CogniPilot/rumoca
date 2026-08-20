//! Workspace symbols handler for Modelica files.

use std::path::Path;

use lsp_types::{Location, Range, SymbolInformation, SymbolKind, Url};
use rumoca_compile::{
    compile::WorkspaceSymbol,
    compile::WorkspaceSymbolKind,
    parsing::{ast, qualify_stored_definition_class_name},
};

use crate::helpers::{location_to_range_in_optional_source, location_to_range_in_source};

/// Handle workspace symbols request - fuzzy search across all documents.
///
/// `source_for_uri` resolves the text of each symbol's *own* file; without it
/// the emitted ranges would be lexer character columns rather than the UTF-16
/// columns LSP requires. A symbol whose file text is unavailable still gets a
/// best-effort range from its character columns.
pub fn handle_workspace_symbols<F>(
    entries: &[WorkspaceSymbol],
    mut source_for_uri: F,
) -> Vec<SymbolInformation>
where
    F: FnMut(&str) -> Option<std::sync::Arc<str>>,
{
    let mut symbols = Vec::with_capacity(entries.len());

    for symbol in entries {
        let Some(uri) = workspace_symbol_uri(&symbol.uri) else {
            continue;
        };
        let kind = match_symbol_kind(&symbol.kind);
        let source = source_for_uri(&symbol.uri);
        let range = location_to_range_in_optional_source(source.as_deref(), &symbol.location);
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
            rumoca_compile::parsing::ir_core::ClassType::Model
            | rumoca_compile::parsing::ir_core::ClassType::Block
            | rumoca_compile::parsing::ir_core::ClassType::Class => SymbolKind::CLASS,
            rumoca_compile::parsing::ir_core::ClassType::Connector => SymbolKind::INTERFACE,
            rumoca_compile::parsing::ir_core::ClassType::Record => SymbolKind::STRUCT,
            rumoca_compile::parsing::ir_core::ClassType::Type => SymbolKind::TYPE_PARAMETER,
            rumoca_compile::parsing::ir_core::ClassType::Package => SymbolKind::NAMESPACE,
            rumoca_compile::parsing::ir_core::ClassType::Function => SymbolKind::FUNCTION,
            rumoca_compile::parsing::ir_core::ClassType::Operator => SymbolKind::OPERATOR,
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
///
/// `source` is the text `ast` was parsed from, so the emitted ranges are UTF-16
/// columns rather than lexer character columns.
pub fn collect_model_names(ast: &ast::StoredDefinition, source: &str) -> Vec<(String, Range)> {
    let mut names = Vec::new();
    for (name, class) in &ast.classes {
        if matches!(
            class.class_type,
            rumoca_compile::parsing::ir_core::ClassType::Model
                | rumoca_compile::parsing::ir_core::ClassType::Block
                | rumoca_compile::parsing::ir_core::ClassType::Class
        ) {
            let range = location_to_range_in_source(source, &class.name.location);
            let model_name = qualify_stored_definition_class_name(ast, name);
            names.push((model_name, range));
        }
    }
    names
}
