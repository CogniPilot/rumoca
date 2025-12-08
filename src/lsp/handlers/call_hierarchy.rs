//! Call Hierarchy handler for Modelica files.
//!
//! Provides call hierarchy support for functions:
//! - Prepare call hierarchy (get item at position)
//! - Incoming calls (who calls this function)
//! - Outgoing calls (what functions does this call)

use std::collections::HashMap;

use lsp_types::{
    CallHierarchyIncomingCall, CallHierarchyIncomingCallsParams, CallHierarchyItem,
    CallHierarchyOutgoingCall, CallHierarchyOutgoingCallsParams, CallHierarchyPrepareParams, Range,
    SymbolKind, Uri,
};

use crate::ir::ast::{ClassDefinition, ClassType, ComponentReference, Expression};
use crate::ir::visitor::{Visitable, Visitor};

use crate::lsp::utils::{get_word_at_position, parse_document, token_to_range};

/// Visitor that finds all calls to a specific target function
struct CallRangeFinder<'a> {
    target: &'a str,
    ranges: Vec<Range>,
}

impl<'a> CallRangeFinder<'a> {
    fn new(target: &'a str) -> Self {
        Self {
            target,
            ranges: Vec::new(),
        }
    }

    fn check_function_call(&mut self, comp: &ComponentReference) {
        if get_function_name(comp) == self.target
            && let Some(ident) = comp.parts.first().map(|p| &p.ident)
        {
            self.ranges.push(token_to_range(ident));
        }
    }
}

impl Visitor for CallRangeFinder<'_> {
    fn enter_expression(&mut self, node: &Expression) {
        if let Expression::FunctionCall { comp, .. } = node {
            self.check_function_call(comp);
        }
    }
}

/// Visitor that collects all function calls with their ranges
struct FunctionCallCollector {
    calls: HashMap<String, Vec<Range>>,
}

impl FunctionCallCollector {
    fn new() -> Self {
        Self {
            calls: HashMap::new(),
        }
    }

    fn record_call(&mut self, comp: &ComponentReference) {
        let name = get_function_name(comp);
        if let Some(ident) = comp.parts.first().map(|p| &p.ident) {
            let range = token_to_range(ident);
            self.calls.entry(name).or_default().push(range);
        }
    }
}

impl Visitor for FunctionCallCollector {
    fn enter_expression(&mut self, node: &Expression) {
        if let Expression::FunctionCall { comp, .. } = node {
            self.record_call(comp);
        }
    }
}

/// Handle prepare call hierarchy request
pub fn handle_prepare_call_hierarchy(
    documents: &HashMap<Uri, String>,
    params: CallHierarchyPrepareParams,
) -> Option<Vec<CallHierarchyItem>> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;
    let text = documents.get(uri)?;
    let path = uri.path().as_str();

    let word = get_word_at_position(text, position)?;
    let ast = parse_document(text, path)?;

    // Find the function/class at this position
    for class in ast.class_list.values() {
        if let Some(item) = find_call_hierarchy_item(class, &word, uri) {
            return Some(vec![item]);
        }
    }

    None
}

/// Handle incoming calls request
pub fn handle_incoming_calls(
    documents: &HashMap<Uri, String>,
    params: CallHierarchyIncomingCallsParams,
) -> Option<Vec<CallHierarchyIncomingCall>> {
    let target_name = &params.item.name;
    let mut calls = Vec::new();

    // Search all documents for calls to this function
    for (uri, text) in documents {
        let path = uri.path().as_str();
        if let Some(ast) = parse_document(text, path) {
            for class in ast.class_list.values() {
                collect_incoming_calls(class, target_name, uri, &mut calls);
            }
        }
    }

    if calls.is_empty() { None } else { Some(calls) }
}

/// Handle outgoing calls request
pub fn handle_outgoing_calls(
    documents: &HashMap<Uri, String>,
    params: CallHierarchyOutgoingCallsParams,
) -> Option<Vec<CallHierarchyOutgoingCall>> {
    let uri = &params.item.uri;
    let text = documents.get(uri)?;
    let path = uri.path().as_str();
    let source_name = &params.item.name;

    let ast = parse_document(text, path)?;
    let mut calls = Vec::new();

    // Find the source function and collect its outgoing calls
    for class in ast.class_list.values() {
        if class.name.text == *source_name {
            collect_outgoing_calls(class, uri, documents, &mut calls);
        }

        // Check nested classes
        for nested in class.classes.values() {
            if nested.name.text == *source_name {
                collect_outgoing_calls(nested, uri, documents, &mut calls);
            }
        }
    }

    if calls.is_empty() { None } else { Some(calls) }
}

/// Find a call hierarchy item for the given name
fn find_call_hierarchy_item(
    class: &ClassDefinition,
    name: &str,
    uri: &Uri,
) -> Option<CallHierarchyItem> {
    // Check if this class matches
    if class.name.text == name {
        let kind = match class.class_type {
            ClassType::Function => SymbolKind::FUNCTION,
            ClassType::Model => SymbolKind::CLASS,
            ClassType::Class => SymbolKind::CLASS,
            ClassType::Record => SymbolKind::STRUCT,
            ClassType::Connector => SymbolKind::INTERFACE,
            ClassType::Package => SymbolKind::MODULE,
            ClassType::Block => SymbolKind::CLASS,
            ClassType::Type => SymbolKind::TYPE_PARAMETER,
            ClassType::Operator => SymbolKind::OPERATOR,
        };

        let range = token_to_range(&class.name);

        return Some(CallHierarchyItem {
            name: class.name.text.clone(),
            kind,
            tags: None,
            detail: Some(format!("{:?}", class.class_type)),
            uri: uri.clone(),
            range,
            selection_range: range,
            data: None,
        });
    }

    // Check nested classes
    for nested in class.classes.values() {
        if let Some(item) = find_call_hierarchy_item(nested, name, uri) {
            return Some(item);
        }
    }

    None
}

/// Collect incoming calls to a target function
fn collect_incoming_calls(
    class: &ClassDefinition,
    target_name: &str,
    uri: &Uri,
    calls: &mut Vec<CallHierarchyIncomingCall>,
) {
    // Use visitor to find all calls to target
    let mut finder = CallRangeFinder::new(target_name);
    class.accept(&mut finder);

    if !finder.ranges.is_empty() {
        let kind = match class.class_type {
            ClassType::Function => SymbolKind::FUNCTION,
            _ => SymbolKind::CLASS,
        };

        let range = token_to_range(&class.name);

        calls.push(CallHierarchyIncomingCall {
            from: CallHierarchyItem {
                name: class.name.text.clone(),
                kind,
                tags: None,
                detail: Some(format!("{:?}", class.class_type)),
                uri: uri.clone(),
                range,
                selection_range: range,
                data: None,
            },
            from_ranges: finder.ranges,
        });
    }

    // Recursively check nested classes
    for nested in class.classes.values() {
        collect_incoming_calls(nested, target_name, uri, calls);
    }
}

/// Collect outgoing calls from a class
fn collect_outgoing_calls(
    class: &ClassDefinition,
    uri: &Uri,
    documents: &HashMap<Uri, String>,
    calls: &mut Vec<CallHierarchyOutgoingCall>,
) {
    // Use visitor to collect all function calls
    let mut collector = FunctionCallCollector::new();
    class.accept(&mut collector);

    // Create outgoing calls for each called function
    for (func_name, from_ranges) in collector.calls {
        // Try to find the function definition
        if let Some(item) = find_function_definition(&func_name, uri, documents) {
            calls.push(CallHierarchyOutgoingCall {
                to: item,
                from_ranges,
            });
        }
    }
}

/// Get function name from a component reference
fn get_function_name(comp: &ComponentReference) -> String {
    comp.parts
        .iter()
        .map(|p| p.ident.text.as_str())
        .collect::<Vec<_>>()
        .join(".")
}

/// Find a function definition in the documents
fn find_function_definition(
    name: &str,
    _current_uri: &Uri,
    documents: &HashMap<Uri, String>,
) -> Option<CallHierarchyItem> {
    for (uri, text) in documents {
        let path = uri.path().as_str();
        if let Some(ast) = parse_document(text, path) {
            for class in ast.class_list.values() {
                if let Some(item) = find_call_hierarchy_item(class, name, uri) {
                    return Some(item);
                }
            }
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::get_function_name;
    use crate::ir::ast::{ComponentRefPart, ComponentReference, Location, Token};

    #[test]
    fn test_get_function_name() {
        let comp_ref = ComponentReference {
            local: false,
            parts: vec![ComponentRefPart {
                ident: Token {
                    text: "sin".to_string(),
                    location: Location::default(),
                    token_number: 0,
                    token_type: 0,
                },
                subs: None,
            }],
        };
        assert_eq!(get_function_name(&comp_ref), "sin".to_string());
    }
}
