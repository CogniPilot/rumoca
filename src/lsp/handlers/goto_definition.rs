//! Go to definition handler for Modelica files.
//!
//! Supports:
//! - Local definitions (variables, parameters, nested classes)
//! - Import aliases (import SI = Modelica.Units.SI)
//! - Cross-file definitions (via workspace state)
//! - Type references in component declarations
//!
//! This module uses canonical scope resolution functions from
//! `crate::ir::transform::scope_resolver` to avoid duplication.

use std::collections::HashMap;

use lsp_types::{GotoDefinitionParams, GotoDefinitionResponse, Location, Position, Range, Uri};

use crate::ir::ast::{ClassDefinition, Name, StoredDefinition, Token};
use crate::ir::transform::scope_resolver::{ImportResolver, ScopeResolver};
use crate::lsp::utils::{
    get_qualified_name_at_position, get_word_at_position, parse_document, token_to_range,
};
use crate::lsp::workspace::WorkspaceState;

/// Handle go to definition request
pub fn handle_goto_definition(
    documents: &HashMap<Uri, String>,
    params: GotoDefinitionParams,
) -> Option<GotoDefinitionResponse> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    let text = documents.get(uri)?;
    let path = uri.path().as_str();

    let word = get_word_at_position(text, position)?;

    // Note: Compilation errors are handled by the diagnostics system, not here.
    // If compilation fails, we fall through to return None (no definition found).
    if let Ok(result) = crate::Compiler::new().compile_str(text, path)
        && let Some(token) = find_definition_in_ast(&result.def, &word)
    {
        return Some(GotoDefinitionResponse::Scalar(Location {
            uri: uri.clone(),
            range: token_to_range(token),
        }));
    }

    None
}

/// Handle go to definition with workspace support for cross-file navigation
pub fn handle_goto_definition_workspace(
    workspace: &mut WorkspaceState,
    params: GotoDefinitionParams,
) -> Option<GotoDefinitionResponse> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;

    let text = workspace.get_document(uri)?;
    let path = uri.path().as_str();

    let word = get_word_at_position(text, position)?;
    let qualified_name = get_qualified_name_at_position(text, position);

    // First try local definition
    if let Some(ast) = parse_document(text, path) {
        // Try qualified name first (e.g., SI.Mass)
        if let Some(ref qn) = qualified_name
            && let Some(response) = try_resolve_qualified_name(&ast, qn, workspace)
        {
            return Some(response);
        }
        // Check for import alias first
        if let Some(import_path) = find_import_alias_in_ast(&ast, &word) {
            // Ensure the package is indexed before lookup
            workspace.ensure_package_indexed(&import_path);
            // Try to resolve the import path in the workspace
            if let Some(sym) = workspace.lookup_symbol(&import_path) {
                return Some(GotoDefinitionResponse::Scalar(Location {
                    uri: sym.uri.clone(),
                    range: Range {
                        start: Position {
                            line: sym.line,
                            character: sym.column,
                        },
                        end: Position {
                            line: sym.line,
                            character: sym.column
                                + import_path.rsplit('.').next().unwrap_or(&import_path).len()
                                    as u32,
                        },
                    },
                }));
            }
        }

        // Check if clicking on the within clause path
        if let Some(within_name) = &ast.within
            && let Some(within_path) = get_within_path_for_word(within_name, &word)
            && let Some(sym) = workspace.lookup_symbol(&within_path)
        {
            return Some(GotoDefinitionResponse::Scalar(Location {
                uri: sym.uri.clone(),
                range: Range {
                    start: Position {
                        line: sym.line,
                        character: sym.column,
                    },
                    end: Position {
                        line: sym.line,
                        character: sym.column + word.len() as u32,
                    },
                },
            }));
        }

        // Check if the word is a type reference that can be resolved with imports
        if let Some(response) = try_resolve_type_with_imports(&ast, &word, workspace, uri) {
            return Some(response);
        }

        if let Some(token) = find_definition_in_ast(&ast, &word) {
            return Some(GotoDefinitionResponse::Scalar(Location {
                uri: uri.clone(),
                range: token_to_range(token),
            }));
        }

        // Try to resolve type using workspace context (considers within clause and imports)
        let resolver = ScopeResolver::new(&ast);
        if let Some(containing_class) =
            resolver.class_at_0indexed(position.line, position.character)
        {
            let class_name = get_qualified_class_name(&ast, &containing_class.name.text);
            if let Some(sym) = workspace.resolve_type(&word, uri, Some(&class_name)) {
                return Some(GotoDefinitionResponse::Scalar(Location {
                    uri: sym.uri.clone(),
                    range: Range {
                        start: Position {
                            line: sym.line,
                            character: sym.column,
                        },
                        end: Position {
                            line: sym.line,
                            character: sym.column + word.len() as u32,
                        },
                    },
                }));
            }
        }
    }

    // Try workspace-wide symbol lookup
    // First check if word is a qualified name or simple name
    if let Some(sym) = workspace.lookup_symbol(&word) {
        return Some(GotoDefinitionResponse::Scalar(Location {
            uri: sym.uri.clone(),
            range: Range {
                start: Position {
                    line: sym.line,
                    character: sym.column,
                },
                end: Position {
                    line: sym.line,
                    character: sym.column + word.len() as u32,
                },
            },
        }));
    }

    // Try looking up by simple name (last part of qualified name)
    let simple_name = word.rsplit('.').next().unwrap_or(&word);
    let matches = workspace.lookup_by_simple_name(simple_name);
    if matches.len() == 1 {
        let sym = matches[0];
        return Some(GotoDefinitionResponse::Scalar(Location {
            uri: sym.uri.clone(),
            range: Range {
                start: Position {
                    line: sym.line,
                    character: sym.column,
                },
                end: Position {
                    line: sym.line,
                    character: sym.column + simple_name.len() as u32,
                },
            },
        }));
    } else if matches.len() > 1 {
        // Multiple matches - return all of them
        let locations: Vec<Location> = matches
            .iter()
            .map(|sym| Location {
                uri: sym.uri.clone(),
                range: Range {
                    start: Position {
                        line: sym.line,
                        character: sym.column,
                    },
                    end: Position {
                        line: sym.line,
                        character: sym.column + simple_name.len() as u32,
                    },
                },
            })
            .collect();
        return Some(GotoDefinitionResponse::Array(locations));
    }

    None
}

/// Get the qualified name for a class considering the within clause
fn get_qualified_class_name(ast: &StoredDefinition, class_name: &str) -> String {
    if let Some(within) = &ast.within {
        format!("{}.{}", within, class_name)
    } else {
        class_name.to_string()
    }
}

/// Try to resolve a qualified name like SI.Mass by resolving import aliases
fn try_resolve_qualified_name(
    ast: &StoredDefinition,
    qualified_name: &str,
    workspace: &mut WorkspaceState,
) -> Option<GotoDefinitionResponse> {
    let parts: Vec<&str> = qualified_name.split('.').collect();
    if parts.is_empty() {
        return None;
    }

    let first_part = parts[0];
    let rest_parts = &parts[1..];

    // Look for import aliases in all classes using canonical ImportResolver
    for class in ast.class_list.values() {
        if let Some(response) =
            try_resolve_import_in_class(class, first_part, rest_parts, workspace)
        {
            return Some(response);
        }
    }

    // Try direct qualified name lookup
    if let Some(sym) = workspace.lookup_symbol(qualified_name) {
        return Some(GotoDefinitionResponse::Scalar(Location {
            uri: sym.uri.clone(),
            range: Range {
                start: Position {
                    line: sym.line,
                    character: sym.column,
                },
                end: Position {
                    line: sym.line,
                    character: sym.column
                        + qualified_name
                            .rsplit('.')
                            .next()
                            .unwrap_or(qualified_name)
                            .len() as u32,
                },
            },
        }));
    }

    None
}

/// Helper to try resolving an import alias in a class and its nested classes
fn try_resolve_import_in_class(
    class: &ClassDefinition,
    first_part: &str,
    rest_parts: &[&str],
    workspace: &mut WorkspaceState,
) -> Option<GotoDefinitionResponse> {
    // Use canonical ImportResolver
    let import_resolver = ImportResolver::from_imports(&class.imports);

    if let Some(resolved_path) = import_resolver.resolve(first_part) {
        // Build the full qualified name by replacing the alias with the resolved path
        let full_qualified = if rest_parts.is_empty() {
            resolved_path.to_string()
        } else {
            format!("{}.{}", resolved_path, rest_parts.join("."))
        };

        // Ensure package is indexed and look up in workspace
        workspace.ensure_package_indexed(&full_qualified);
        if let Some(sym) = workspace.lookup_symbol(&full_qualified) {
            return Some(GotoDefinitionResponse::Scalar(Location {
                uri: sym.uri.clone(),
                range: Range {
                    start: Position {
                        line: sym.line,
                        character: sym.column,
                    },
                    end: Position {
                        line: sym.line,
                        character: sym.column
                            + full_qualified
                                .rsplit('.')
                                .next()
                                .unwrap_or(&full_qualified)
                                .len() as u32,
                    },
                },
            }));
        }

        // Try simple name lookup as fallback
        let simple = full_qualified.rsplit('.').next().unwrap_or(&full_qualified);
        let matches = workspace.lookup_by_simple_name(simple);
        if matches.len() == 1 {
            let sym = matches[0];
            return Some(GotoDefinitionResponse::Scalar(Location {
                uri: sym.uri.clone(),
                range: Range {
                    start: Position {
                        line: sym.line,
                        character: sym.column,
                    },
                    end: Position {
                        line: sym.line,
                        character: sym.column + simple.len() as u32,
                    },
                },
            }));
        }
    }

    // Recursively check nested classes
    for nested in class.classes.values() {
        if let Some(response) =
            try_resolve_import_in_class(nested, first_part, rest_parts, workspace)
        {
            return Some(response);
        }
    }

    None
}

/// Try to resolve a type name using imports in the AST (uses canonical ImportResolver)
fn try_resolve_type_with_imports(
    ast: &StoredDefinition,
    word: &str,
    workspace: &mut WorkspaceState,
    _uri: &Uri,
) -> Option<GotoDefinitionResponse> {
    // Check all imports in all classes to see if the word matches an import
    for class in ast.class_list.values() {
        if let Some(response) = try_resolve_in_class_imports(class, word, workspace) {
            return Some(response);
        }
    }
    None
}

/// Try to resolve a type name using imports in a specific class (uses canonical ImportResolver)
fn try_resolve_in_class_imports(
    class: &ClassDefinition,
    word: &str,
    workspace: &mut WorkspaceState,
) -> Option<GotoDefinitionResponse> {
    // Use canonical ImportResolver for qualified/renamed/selective imports
    let import_resolver = ImportResolver::from_imports(&class.imports);

    if let Some(resolved_path) = import_resolver.resolve(word) {
        workspace.ensure_package_indexed(resolved_path);
        if let Some(sym) = workspace.lookup_symbol(resolved_path) {
            return Some(GotoDefinitionResponse::Scalar(Location {
                uri: sym.uri.clone(),
                range: Range {
                    start: Position {
                        line: sym.line,
                        character: sym.column,
                    },
                    end: Position {
                        line: sym.line,
                        character: sym.column + word.len() as u32,
                    },
                },
            }));
        }
    }

    // Handle unqualified imports (import A.B.*;) separately since ImportResolver
    // can't pre-resolve these without knowing available symbols
    for import in &class.imports {
        if let crate::ir::ast::Import::Unqualified { path, .. } = import {
            let qualified = format!("{}.{}", path, word);
            workspace.ensure_package_indexed(&qualified);
            if let Some(sym) = workspace.lookup_symbol(&qualified) {
                return Some(GotoDefinitionResponse::Scalar(Location {
                    uri: sym.uri.clone(),
                    range: Range {
                        start: Position {
                            line: sym.line,
                            character: sym.column,
                        },
                        end: Position {
                            line: sym.line,
                            character: sym.column + word.len() as u32,
                        },
                    },
                }));
            }
        }
    }

    // Recursively check nested classes
    for nested in class.classes.values() {
        if let Some(response) = try_resolve_in_class_imports(nested, word, workspace) {
            return Some(response);
        }
    }

    None
}

/// Find a definition in the AST, returning the Token if found
fn find_definition_in_ast<'a>(def: &'a StoredDefinition, name: &str) -> Option<&'a Token> {
    for class in def.class_list.values() {
        if let Some(token) = find_definition_in_class(class, name) {
            return Some(token);
        }
    }
    None
}

/// Recursively search for a definition in a class
fn find_definition_in_class<'a>(class: &'a ClassDefinition, name: &str) -> Option<&'a Token> {
    if class.name.text == name {
        return Some(&class.name);
    }

    for (comp_name, comp) in &class.components {
        if comp_name == name {
            return Some(&comp.name_token);
        }
    }

    for nested_class in class.classes.values() {
        if let Some(token) = find_definition_in_class(nested_class, name) {
            return Some(token);
        }
    }

    None
}

/// Find an import alias in the AST and return the path it resolves to (uses canonical ImportResolver)
fn find_import_alias_in_ast(def: &StoredDefinition, alias: &str) -> Option<String> {
    for class in def.class_list.values() {
        if let Some(path) = find_import_alias_in_class(class, alias) {
            return Some(path);
        }
    }
    None
}

/// Recursively search for an import alias in a class (uses canonical ImportResolver)
fn find_import_alias_in_class(class: &ClassDefinition, alias: &str) -> Option<String> {
    // Use canonical ImportResolver
    let import_resolver = ImportResolver::from_imports(&class.imports);
    if let Some(path) = import_resolver.resolve(alias) {
        return Some(path.to_string());
    }

    // Check nested classes
    for nested_class in class.classes.values() {
        if let Some(path) = find_import_alias_in_class(nested_class, alias) {
            return Some(path);
        }
    }

    None
}

/// Get the full path from a within clause if the word matches any part of it
/// For example, if within is "Modelica.Blocks" and word is "Blocks",
/// returns "Modelica.Blocks"
fn get_within_path_for_word(within_name: &Name, word: &str) -> Option<String> {
    let mut path_parts = Vec::new();
    for token in &within_name.name {
        path_parts.push(token.text.as_str());
        if token.text == word {
            // Found the word, return the path up to this point
            return Some(path_parts.join("."));
        }
    }
    None
}
