//! Code completion handler for Modelica files.
//!
//! Provides:
//! - Local completions (variables, types in current file)
//! - Workspace-wide completions (symbols from all open files)
//! - Package/import completions

mod members;
mod modifiers;
mod scope;
mod workspace;

use std::collections::HashMap;

use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse, InsertTextFormat, Uri,
};

use crate::lsp::data::builtin_functions::get_builtin_functions;
use crate::lsp::data::keywords::get_keyword_completions;
use crate::lsp::utils::{get_text_before_cursor, parse_document};
use crate::lsp::workspace::WorkspaceState;

use members::get_member_completions;
use modifiers::get_modifier_completions;
use scope::get_scoped_completions;
use workspace::{
    get_workspace_completions, get_workspace_member_completions, is_in_import_context,
};

/// Handle completion request with workspace support
///
/// Provides completions from:
/// - Local scope (variables, types in current file)
/// - Workspace symbols (classes, models from other files)
/// - Package/import completions
/// - Built-in functions and keywords
/// - Modifier completions (start, fixed, min, max, etc.)
pub fn handle_completion_workspace(
    workspace: &WorkspaceState,
    params: CompletionParams,
) -> Option<CompletionResponse> {
    let uri = &params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;
    let text = workspace.get_document(uri)?;
    let path = uri.path().as_str();

    let mut items = Vec::new();

    // Check if we're doing dot completion or import completion
    let text_before = get_text_before_cursor(text, position)?;
    let is_dot_completion = text_before.ends_with('.');
    let is_import = is_in_import_context(&text_before);

    // Check if we're in a modifier context (inside parentheses after type declaration)
    // Try parsing for class lookup (for class instance member modifiers)
    let ast_for_modifiers =
        parse_document(text, path).or_else(|| workspace.get_cached_ast(uri).cloned());
    if let Some(modifier_items) = get_modifier_completions(&text_before, ast_for_modifiers.as_ref())
    {
        return Some(CompletionResponse::Array(modifier_items));
    }

    if is_dot_completion {
        let before_dot = &text_before[..text_before.len() - 1];
        let prefix: String = before_dot
            .chars()
            .rev()
            .take_while(|c| c.is_alphanumeric() || *c == '_' || *c == '.')
            .collect::<String>()
            .chars()
            .rev()
            .collect();

        if !prefix.is_empty() {
            // Try local AST first - this handles component member access (e.g., ball.h)
            // First try parsing current text, then fall back to cached AST
            let ast_option = parse_document(text, path).or_else(|| {
                // Current parse failed (syntax error while typing), use cached AST
                workspace.get_cached_ast(uri).cloned()
            });

            if let Some(ast) = ast_option {
                items.extend(get_member_completions(
                    &ast,
                    &format!("{}.", prefix),
                    position,
                ));
            }

            // Only get workspace package completions if we didn't find local members
            // This handles package navigation (e.g., Modelica.Math.) but not
            // local variable member access (e.g., ball.)
            if items.is_empty() {
                items.extend(get_workspace_member_completions(workspace, &prefix));
            }
        }

        // For dot completion, only return the member items (no keywords/functions)
        return Some(CompletionResponse::Array(items));
    }

    // Get scoped completions from the AST
    if let Some(ast) = parse_document(text, path) {
        items.extend(get_scoped_completions(&ast, position));
    }

    // Add workspace symbols (classes, models from other files)
    items.extend(get_workspace_completions(workspace, is_import));

    // Add built-in functions with snippets
    items.extend(get_builtin_function_completions());

    // Modelica keywords
    items.extend(get_keyword_completions());

    Some(CompletionResponse::Array(items))
}

/// Handle completion request (non-workspace version for backwards compatibility)
pub fn handle_completion(
    documents: &HashMap<Uri, String>,
    params: CompletionParams,
) -> Option<CompletionResponse> {
    let uri = &params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;
    let text = documents.get(uri)?;
    let path = uri.path().as_str();

    let mut items = Vec::new();

    // Check if we're doing dot completion
    let text_before = get_text_before_cursor(text, position)?;
    let is_dot_completion = text_before.ends_with('.');

    // Check if we're in a modifier context (inside parentheses after type declaration)
    let ast_for_modifiers = parse_document(text, path);
    if let Some(modifier_items) = get_modifier_completions(&text_before, ast_for_modifiers.as_ref())
    {
        return Some(CompletionResponse::Array(modifier_items));
    }

    if is_dot_completion {
        let before_dot = &text_before[..text_before.len() - 1];
        let prefix: String = before_dot
            .chars()
            .rev()
            .take_while(|c| c.is_alphanumeric() || *c == '_' || *c == '.')
            .collect::<String>()
            .chars()
            .rev()
            .collect();

        if !prefix.is_empty() {
            if let Some(ast) = parse_document(text, path) {
                items.extend(get_member_completions(
                    &ast,
                    &format!("{}.", prefix),
                    position,
                ));
            }
        }

        if items.is_empty() {
            return Some(CompletionResponse::Array(vec![]));
        }

        return Some(CompletionResponse::Array(items));
    }

    // Get scoped completions from the AST
    if let Some(ast) = parse_document(text, path) {
        items.extend(get_scoped_completions(&ast, position));
    }

    // Add built-in functions with snippets
    items.extend(get_builtin_function_completions());

    // Modelica keywords
    items.extend(get_keyword_completions());

    Some(CompletionResponse::Array(items))
}

/// Get completion items for built-in functions
fn get_builtin_function_completions() -> Vec<CompletionItem> {
    let functions = get_builtin_functions();
    functions
        .iter()
        .map(|func| {
            let snippet = if func.parameters.is_empty() {
                format!("{}()", func.name)
            } else {
                let params: Vec<String> = func
                    .parameters
                    .iter()
                    .enumerate()
                    .map(|(i, (name, _))| format!("${{{}:{}}}", i + 1, name))
                    .collect();
                format!("{}({})", func.name, params.join(", "))
            };

            CompletionItem {
                label: func.name.to_string(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some(func.signature.to_string()),
                documentation: Some(lsp_types::Documentation::String(
                    func.documentation.to_string(),
                )),
                insert_text: Some(snippet),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                ..Default::default()
            }
        })
        .collect()
}
