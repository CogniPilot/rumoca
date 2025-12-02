//! Go to Type Definition handler for Modelica files.
//!
//! Navigates from a component to its type definition:
//! - `Motor motor` → jump to `model Motor`
//! - `MyConnector c` → jump to `connector MyConnector`

use std::collections::HashMap;

use lsp_types::{GotoDefinitionResponse, Location, TextDocumentPositionParams, Uri};

use crate::ir::ast::{ClassDefinition, StoredDefinition, Token};

use crate::lsp::utils::{get_word_at_position, parse_document, token_to_range};

/// Handle go to type definition request
pub fn handle_type_definition(
    documents: &HashMap<Uri, String>,
    params: TextDocumentPositionParams,
) -> Option<GotoDefinitionResponse> {
    let uri = &params.text_document.uri;
    let position = params.position;

    let text = documents.get(uri)?;
    let path = uri.path().as_str();

    let word = get_word_at_position(text, position)?;
    let ast = parse_document(text, path)?;

    // First, find if the word is a component name and get its type
    if let Some(type_name) = find_component_type(&ast, &word) {
        // Now find the type definition
        if let Some(token) = find_type_definition(&ast, &type_name) {
            return Some(GotoDefinitionResponse::Scalar(Location {
                uri: uri.clone(),
                range: token_to_range(token),
            }));
        }
    }

    // If the word itself is a type name, try to find its definition
    if let Some(token) = find_type_definition(&ast, &word) {
        return Some(GotoDefinitionResponse::Scalar(Location {
            uri: uri.clone(),
            range: token_to_range(token),
        }));
    }

    None
}

/// Find the type name for a component in the AST
fn find_component_type(def: &StoredDefinition, component_name: &str) -> Option<String> {
    for class in def.class_list.values() {
        if let Some(type_name) = find_component_type_in_class(class, component_name) {
            return Some(type_name);
        }
    }
    None
}

/// Recursively search for a component's type in a class
fn find_component_type_in_class(class: &ClassDefinition, component_name: &str) -> Option<String> {
    // Check if any component matches
    for (comp_name, comp) in &class.components {
        if comp_name == component_name {
            // Return the type name (first part of qualified name)
            return Some(comp.type_name.to_string());
        }
    }

    // Check nested classes
    for nested_class in class.classes.values() {
        if let Some(type_name) = find_component_type_in_class(nested_class, component_name) {
            return Some(type_name);
        }
    }

    None
}

/// Find the definition location of a type in the AST
fn find_type_definition<'a>(def: &'a StoredDefinition, type_name: &str) -> Option<&'a Token> {
    // Handle qualified names (e.g., "Modelica.SIunits.Voltage")
    let base_type = type_name.rsplit('.').next().unwrap_or(type_name);

    for class in def.class_list.values() {
        if let Some(token) = find_type_in_class(class, base_type) {
            return Some(token);
        }
    }
    None
}

/// Recursively search for a type definition in a class
fn find_type_in_class<'a>(class: &'a ClassDefinition, type_name: &str) -> Option<&'a Token> {
    // Check if this class matches the type name
    if class.name.text == type_name {
        return Some(&class.name);
    }

    // Check nested classes (including type definitions, records, etc.)
    for (nested_name, nested_class) in &class.classes {
        if nested_name == type_name {
            return Some(&nested_class.name);
        }
        // Recursively search in nested classes
        if let Some(token) = find_type_in_class(nested_class, type_name) {
            return Some(token);
        }
    }

    None
}
