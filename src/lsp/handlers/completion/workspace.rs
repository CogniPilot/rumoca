//! Workspace-wide completion handling.
//!
//! Provides completions from workspace symbols (classes, models from other files).

use lsp_types::CompletionItem;
use lsp_types::CompletionItemKind;

use crate::lsp::workspace::{SymbolKind, WorkspaceState};

/// Check if the cursor is in an import statement context
pub fn is_in_import_context(text_before: &str) -> bool {
    // Look for "import" keyword before the cursor (on the same line)
    let lines: Vec<&str> = text_before.lines().collect();
    if let Some(last_line) = lines.last() {
        let trimmed = last_line.trim();
        // Match: "import", "import ", "import Foo", "import Modelica.Blocks"
        // But not if we're past a semicolon (completed import)
        if trimmed.contains(';') {
            return false;
        }
        // Check if line starts with "import" keyword
        if trimmed == "import" || trimmed.starts_with("import ") {
            return true;
        }
    }
    false
}

/// Get completions from workspace symbols
pub fn get_workspace_completions(
    workspace: &WorkspaceState,
    is_import_context: bool,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    let mut seen = std::collections::HashSet::new();

    // Get all workspace symbols
    for symbol in workspace.find_symbols("") {
        // Skip if we've already added this symbol
        if !seen.insert(&symbol.qualified_name) {
            continue;
        }

        // Never show components/variables - they're not importable and shouldn't
        // pollute completions from other files
        match symbol.kind {
            SymbolKind::Component | SymbolKind::Parameter | SymbolKind::Constant => continue,
            _ => {}
        }

        // For non-import context, only show top-level symbols
        // Nested symbols like NestedTestPackage.Controllers.pid should not appear
        // as "pid" in completions - they're not in scope unless imported
        if !is_import_context && symbol.qualified_name.contains('.') {
            continue;
        }

        let kind = symbol_kind_to_completion_kind(symbol.kind);

        // For import context, prefer showing qualified names
        // For other contexts, show simple names with qualified name in detail
        let simple_name = symbol
            .qualified_name
            .rsplit('.')
            .next()
            .unwrap_or(&symbol.qualified_name);

        if is_import_context {
            // In import context, show full qualified name
            items.push(CompletionItem {
                label: symbol.qualified_name.clone(),
                kind: Some(kind),
                detail: symbol.detail.clone(),
                ..Default::default()
            });
        } else {
            // In normal context, show simple name with qualified in detail
            items.push(CompletionItem {
                label: simple_name.to_string(),
                kind: Some(kind),
                detail: Some(symbol.qualified_name.clone()),
                filter_text: Some(format!("{} {}", simple_name, symbol.qualified_name)),
                ..Default::default()
            });
        }
    }

    // In import context, also add top-level package names from MODELICAPATH
    if is_import_context {
        items.extend(get_modelica_path_packages(workspace));
    }

    items
}

/// Get top-level package names from MODELICAPATH for import completion
fn get_modelica_path_packages(workspace: &WorkspaceState) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    let mut seen = std::collections::HashSet::new();

    // Get top-level packages from indexed symbols
    for symbol in workspace.find_symbols("") {
        // Extract top-level package name (first component of qualified name)
        let top_level = symbol
            .qualified_name
            .split('.')
            .next()
            .unwrap_or(&symbol.qualified_name);

        if seen.insert(top_level.to_string()) {
            items.push(CompletionItem {
                label: top_level.to_string(),
                kind: Some(CompletionItemKind::MODULE),
                detail: Some("Package".to_string()),
                ..Default::default()
            });
        }
    }

    items
}

/// Get member completions from workspace symbols for dot completion
pub fn get_workspace_member_completions(
    workspace: &WorkspaceState,
    prefix: &str,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    let prefix_with_dot = format!("{}.", prefix);

    // Find all symbols that start with the prefix
    for symbol in workspace.find_symbols("") {
        if symbol.qualified_name.starts_with(&prefix_with_dot) {
            // Get the part after the prefix
            let remainder = &symbol.qualified_name[prefix_with_dot.len()..];

            // Only show direct children (no more dots in remainder)
            if !remainder.contains('.') {
                let kind = symbol_kind_to_completion_kind(symbol.kind);

                items.push(CompletionItem {
                    label: remainder.to_string(),
                    kind: Some(kind),
                    detail: symbol.detail.clone(),
                    ..Default::default()
                });
            }
        }
    }

    items
}

/// Convert workspace SymbolKind to LSP CompletionItemKind
fn symbol_kind_to_completion_kind(kind: SymbolKind) -> CompletionItemKind {
    match kind {
        SymbolKind::Package => CompletionItemKind::MODULE,
        SymbolKind::Model => CompletionItemKind::CLASS,
        SymbolKind::Class => CompletionItemKind::CLASS,
        SymbolKind::Block => CompletionItemKind::CLASS,
        SymbolKind::Connector => CompletionItemKind::INTERFACE,
        SymbolKind::Record => CompletionItemKind::STRUCT,
        SymbolKind::Type => CompletionItemKind::TYPE_PARAMETER,
        SymbolKind::Function => CompletionItemKind::FUNCTION,
        SymbolKind::Operator => CompletionItemKind::OPERATOR,
        SymbolKind::Component => CompletionItemKind::FIELD,
        SymbolKind::Parameter => CompletionItemKind::CONSTANT,
        SymbolKind::Constant => CompletionItemKind::CONSTANT,
    }
}
