//! Workspace Symbol Search handler for Modelica files.
//!
//! Provides global symbol search (`Ctrl+T` / `Cmd+T`):
//! - Search all models, functions, connectors in workspace
//! - Fuzzy matching support
//! - Show symbol kind and location

use std::collections::HashMap;

use lsp_types::{Location, SymbolInformation, SymbolKind, Uri, WorkspaceSymbolParams};

use crate::ir::ast::{ClassDefinition, ClassType, StoredDefinition, Variability};

use crate::lsp::utils::{parse_document, token_to_range};

/// Handle workspace symbol request
#[allow(deprecated)] // SymbolInformation::deprecated field is deprecated but required
pub fn handle_workspace_symbol(
    documents: &HashMap<Uri, String>,
    params: WorkspaceSymbolParams,
) -> Option<Vec<SymbolInformation>> {
    let query = params.query.to_lowercase();
    let mut symbols = Vec::new();

    for (uri, text) in documents {
        let path = uri.path().as_str();
        if let Some(ast) = parse_document(text, path) {
            collect_symbols_from_ast(&ast, uri, &query, &mut symbols);
        }
    }

    // Sort by relevance (exact matches first, then prefix matches, then contains)
    symbols.sort_by(|a, b| {
        let a_name = a.name.to_lowercase();
        let b_name = b.name.to_lowercase();

        // Exact match gets highest priority
        let a_exact = a_name == query;
        let b_exact = b_name == query;
        if a_exact != b_exact {
            return b_exact.cmp(&a_exact);
        }

        // Prefix match gets second priority
        let a_prefix = a_name.starts_with(&query);
        let b_prefix = b_name.starts_with(&query);
        if a_prefix != b_prefix {
            return b_prefix.cmp(&a_prefix);
        }

        // Otherwise sort alphabetically
        a_name.cmp(&b_name)
    });

    Some(symbols)
}

/// Collect all symbols from the AST that match the query
#[allow(deprecated)] // SymbolInformation::deprecated field is deprecated but required
fn collect_symbols_from_ast(
    def: &StoredDefinition,
    uri: &Uri,
    query: &str,
    symbols: &mut Vec<SymbolInformation>,
) {
    for class in def.class_list.values() {
        collect_symbols_from_class(class, uri, query, symbols, None);
    }
}

/// Recursively collect symbols from a class definition
#[allow(deprecated)] // SymbolInformation::deprecated field is deprecated but required
fn collect_symbols_from_class(
    class: &ClassDefinition,
    uri: &Uri,
    query: &str,
    symbols: &mut Vec<SymbolInformation>,
    container: Option<String>,
) {
    let class_name = &class.name.text;

    // Check if this class matches the query (fuzzy match)
    if matches_query(class_name, query) {
        let kind = class_type_to_symbol_kind(&class.class_type);
        let location = Location {
            uri: uri.clone(),
            range: token_to_range(&class.name),
        };

        symbols.push(SymbolInformation {
            name: class_name.clone(),
            kind,
            tags: None,
            deprecated: None,
            location,
            container_name: container.clone(),
        });
    }

    // Collect components (variables, parameters)
    for (comp_name, comp) in &class.components {
        if matches_query(comp_name, query) {
            let kind = match &comp.variability {
                Variability::Parameter(_) | Variability::Constant(_) => SymbolKind::CONSTANT,
                _ => SymbolKind::VARIABLE,
            };

            let location = Location {
                uri: uri.clone(),
                range: token_to_range(&comp.name_token),
            };

            symbols.push(SymbolInformation {
                name: comp_name.clone(),
                kind,
                tags: None,
                deprecated: None,
                location,
                container_name: Some(class_name.clone()),
            });
        }
    }

    // Recursively process nested classes
    let container_name = match &container {
        Some(c) => format!("{}.{}", c, class_name),
        None => class_name.clone(),
    };

    for nested_class in class.classes.values() {
        collect_symbols_from_class(
            nested_class,
            uri,
            query,
            symbols,
            Some(container_name.clone()),
        );
    }
}

/// Check if a name matches the query using fuzzy matching
fn matches_query(name: &str, query: &str) -> bool {
    if query.is_empty() {
        return true;
    }

    let name_lower = name.to_lowercase();

    // Exact match
    if name_lower == query {
        return true;
    }

    // Prefix match
    if name_lower.starts_with(query) {
        return true;
    }

    // Contains match
    if name_lower.contains(query) {
        return true;
    }

    // Fuzzy match: all query characters appear in order in name
    let mut query_chars = query.chars().peekable();
    for name_char in name_lower.chars() {
        if let Some(&query_char) = query_chars.peek()
            && name_char == query_char
        {
            query_chars.next();
        }
    }
    query_chars.peek().is_none()
}

/// Convert ClassType to LSP SymbolKind
fn class_type_to_symbol_kind(class_type: &ClassType) -> SymbolKind {
    match class_type {
        ClassType::Model => SymbolKind::CLASS,
        ClassType::Class => SymbolKind::CLASS,
        ClassType::Block => SymbolKind::CLASS,
        ClassType::Connector => SymbolKind::INTERFACE,
        ClassType::Type => SymbolKind::TYPE_PARAMETER,
        ClassType::Package => SymbolKind::NAMESPACE,
        ClassType::Function => SymbolKind::FUNCTION,
        ClassType::Record => SymbolKind::STRUCT,
        ClassType::Operator => SymbolKind::OPERATOR,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_matches_query_empty() {
        assert!(matches_query("Motor", ""));
        assert!(matches_query("anything", ""));
    }

    #[test]
    fn test_matches_query_exact() {
        assert!(matches_query("Motor", "motor"));
        assert!(matches_query("motor", "motor"));
    }

    #[test]
    fn test_matches_query_prefix() {
        assert!(matches_query("MotorController", "motor"));
        assert!(matches_query("MotorController", "motorc"));
    }

    #[test]
    fn test_matches_query_contains() {
        assert!(matches_query("DCMotor", "motor"));
        assert!(matches_query("MyMotorClass", "motor"));
    }

    #[test]
    fn test_matches_query_fuzzy() {
        assert!(matches_query("MotorController", "mc"));
        assert!(matches_query("MotorController", "mtc"));
        assert!(matches_query("SomeVeryLongName", "svln"));
    }

    #[test]
    fn test_matches_query_no_match() {
        assert!(!matches_query("Motor", "xyz"));
        assert!(!matches_query("Motor", "motorz"));
    }
}
