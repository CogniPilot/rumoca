//! Workspace symbols handler for Modelica files.

use lsp_types::{Location, Position, Range, SymbolInformation, SymbolKind, Url};
use rumoca_ir_ast as ast;

use crate::helpers::location_to_range;

/// Workspace symbol entry from a document.
pub struct DocSymbols<'a> {
    pub uri: &'a str,
    pub ast: &'a ast::StoredDefinition,
}

/// Handle workspace symbols request - fuzzy search across all documents.
pub fn handle_workspace_symbols(docs: &[DocSymbols<'_>], query: &str) -> Vec<SymbolInformation> {
    let query_lower = query.to_lowercase();
    let mut symbols = Vec::new();

    for doc in docs {
        let uri = match Url::parse(doc.uri).or_else(|_| Url::parse(&format!("file://{}", doc.uri)))
        {
            Ok(u) => u,
            Err(_) => continue,
        };
        collect_symbols(doc.ast, &uri, &query_lower, &mut symbols);
    }

    // Sort: exact match > prefix > contains
    symbols.sort_by(|a, b| {
        let a_score = match_score(&a.name, &query_lower);
        let b_score = match_score(&b.name, &query_lower);
        a_score.cmp(&b_score)
    });

    symbols
}

fn match_score(name: &str, query: &str) -> u8 {
    let name_lower = name.to_lowercase();
    if name_lower == query {
        0
    } else if name_lower.starts_with(query) {
        1
    } else {
        2
    }
}

fn collect_symbols(
    ast: &ast::StoredDefinition,
    uri: &Url,
    query: &str,
    symbols: &mut Vec<SymbolInformation>,
) {
    for (name, class) in &ast.classes {
        collect_class_symbols(name, class, uri, query, None, symbols);
    }
}

fn collect_class_symbols(
    name: &str,
    class: &ast::ClassDef,
    uri: &Url,
    query: &str,
    container: Option<String>,
    symbols: &mut Vec<SymbolInformation>,
) {
    let name_lower = name.to_lowercase();
    if query.is_empty() || name_lower.contains(query) {
        let kind = match class.class_type {
            ast::ClassType::Model | ast::ClassType::Block | ast::ClassType::Class => {
                SymbolKind::CLASS
            }
            ast::ClassType::Connector => SymbolKind::INTERFACE,
            ast::ClassType::Record => SymbolKind::STRUCT,
            ast::ClassType::Type => SymbolKind::TYPE_PARAMETER,
            ast::ClassType::Package => SymbolKind::NAMESPACE,
            ast::ClassType::Function => SymbolKind::FUNCTION,
            ast::ClassType::Operator => SymbolKind::OPERATOR,
        };

        let range = location_to_range(&class.location);

        symbols.push(new_symbol_information(
            name.to_string(),
            kind,
            Location {
                uri: uri.clone(),
                range,
            },
            container.clone(),
        ));
    }

    // Components
    for (comp_name, comp) in &class.components {
        let comp_lower = comp_name.to_lowercase();
        if query.is_empty() || comp_lower.contains(query) {
            let range = location_to_range(&comp.location);
            symbols.push(new_symbol_information(
                comp_name.clone(),
                SymbolKind::VARIABLE,
                Location {
                    uri: uri.clone(),
                    range,
                },
                Some(name.to_string()),
            ));
        }
    }

    // Nested classes
    for (nested_name, nested) in &class.classes {
        collect_class_symbols(
            nested_name,
            nested,
            uri,
            query,
            Some(name.to_string()),
            symbols,
        );
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
