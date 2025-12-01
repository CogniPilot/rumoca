//! Call Hierarchy handler for Modelica files.
//!
//! Provides call hierarchy support for functions:
//! - Prepare call hierarchy (get item at position)
//! - Incoming calls (who calls this function)
//! - Outgoing calls (what functions does this call)

// Allow mutable key type warning - Uri has interior mutability but we use it correctly
#![allow(clippy::mutable_key_type)]

use std::collections::HashMap;

use lsp_types::{
    CallHierarchyIncomingCall, CallHierarchyIncomingCallsParams, CallHierarchyItem,
    CallHierarchyOutgoingCall, CallHierarchyOutgoingCallsParams, CallHierarchyPrepareParams,
    Position, Range, SymbolKind, Uri,
};

use crate::ir::ast::{ClassDefinition, ClassType, ComponentReference, Equation, Expression, Statement};

use super::utils::{get_word_at_position, parse_document};

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

    if calls.is_empty() {
        None
    } else {
        Some(calls)
    }
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

    if calls.is_empty() {
        None
    } else {
        Some(calls)
    }
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

        let line = class.name.location.start_line.saturating_sub(1);
        let col = class.name.location.start_column.saturating_sub(1);

        return Some(CallHierarchyItem {
            name: class.name.text.clone(),
            kind,
            tags: None,
            detail: Some(format!("{:?}", class.class_type)),
            uri: uri.clone(),
            range: Range {
                start: Position { line, character: col },
                end: Position {
                    line,
                    character: col + class.name.text.len() as u32,
                },
            },
            selection_range: Range {
                start: Position { line, character: col },
                end: Position {
                    line,
                    character: col + class.name.text.len() as u32,
                },
            },
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
    let mut from_ranges = Vec::new();

    // Check equations for function calls
    for eq in &class.equations {
        collect_call_ranges_from_equation(eq, target_name, &mut from_ranges);
    }

    for eq in &class.initial_equations {
        collect_call_ranges_from_equation(eq, target_name, &mut from_ranges);
    }

    // Check algorithms
    for algo in &class.algorithms {
        for stmt in algo {
            collect_call_ranges_from_statement(stmt, target_name, &mut from_ranges);
        }
    }

    for algo in &class.initial_algorithms {
        for stmt in algo {
            collect_call_ranges_from_statement(stmt, target_name, &mut from_ranges);
        }
    }

    // Check component start expressions for function calls
    for comp in class.components.values() {
        if !matches!(comp.start, Expression::Empty) {
            collect_call_ranges_from_expression(&comp.start, target_name, &mut from_ranges);
        }
    }

    if !from_ranges.is_empty() {
        let line = class.name.location.start_line.saturating_sub(1);
        let col = class.name.location.start_column.saturating_sub(1);

        let kind = match class.class_type {
            ClassType::Function => SymbolKind::FUNCTION,
            _ => SymbolKind::CLASS,
        };

        calls.push(CallHierarchyIncomingCall {
            from: CallHierarchyItem {
                name: class.name.text.clone(),
                kind,
                tags: None,
                detail: Some(format!("{:?}", class.class_type)),
                uri: uri.clone(),
                range: Range {
                    start: Position { line, character: col },
                    end: Position {
                        line,
                        character: col + class.name.text.len() as u32,
                    },
                },
                selection_range: Range {
                    start: Position { line, character: col },
                    end: Position {
                        line,
                        character: col + class.name.text.len() as u32,
                    },
                },
                data: None,
            },
            from_ranges,
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
    let mut called_functions: HashMap<String, Vec<Range>> = HashMap::new();

    // Collect all function calls
    for eq in &class.equations {
        collect_function_calls_from_equation(eq, &mut called_functions);
    }

    for eq in &class.initial_equations {
        collect_function_calls_from_equation(eq, &mut called_functions);
    }

    for algo in &class.algorithms {
        for stmt in algo {
            collect_function_calls_from_statement(stmt, &mut called_functions);
        }
    }

    for algo in &class.initial_algorithms {
        for stmt in algo {
            collect_function_calls_from_statement(stmt, &mut called_functions);
        }
    }

    // Create outgoing calls for each called function
    for (func_name, from_ranges) in called_functions {
        // Try to find the function definition
        if let Some(item) = find_function_definition(&func_name, uri, documents) {
            calls.push(CallHierarchyOutgoingCall {
                to: item,
                from_ranges,
            });
        }
    }
}

/// Collect call ranges from an equation
fn collect_call_ranges_from_equation(eq: &Equation, target: &str, ranges: &mut Vec<Range>) {
    match eq {
        Equation::Simple { lhs, rhs } => {
            collect_call_ranges_from_expression(lhs, target, ranges);
            collect_call_ranges_from_expression(rhs, target, ranges);
        }
        Equation::For { equations, .. } => {
            for sub_eq in equations {
                collect_call_ranges_from_equation(sub_eq, target, ranges);
            }
        }
        Equation::If { cond_blocks, else_block } => {
            for block in cond_blocks {
                collect_call_ranges_from_expression(&block.cond, target, ranges);
                for eq in &block.eqs {
                    collect_call_ranges_from_equation(eq, target, ranges);
                }
            }
            if let Some(else_eqs) = else_block {
                for eq in else_eqs {
                    collect_call_ranges_from_equation(eq, target, ranges);
                }
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                collect_call_ranges_from_expression(&block.cond, target, ranges);
                for eq in &block.eqs {
                    collect_call_ranges_from_equation(eq, target, ranges);
                }
            }
        }
        Equation::FunctionCall { comp, args } => {
            if get_function_name(comp) == target {
                if let Some(loc) = comp.parts.first().map(|p| &p.ident.location) {
                    ranges.push(Range {
                        start: Position {
                            line: loc.start_line.saturating_sub(1),
                            character: loc.start_column.saturating_sub(1),
                        },
                        end: Position {
                            line: loc.start_line.saturating_sub(1),
                            character: loc.start_column.saturating_sub(1) + target.len() as u32,
                        },
                    });
                }
            }
            for arg in args {
                collect_call_ranges_from_expression(arg, target, ranges);
            }
        }
        _ => {}
    }
}

/// Collect call ranges from a statement
fn collect_call_ranges_from_statement(stmt: &Statement, target: &str, ranges: &mut Vec<Range>) {
    match stmt {
        Statement::Assignment { value, .. } => {
            collect_call_ranges_from_expression(value, target, ranges);
        }
        Statement::For { equations, .. } => {
            for sub_stmt in equations {
                collect_call_ranges_from_statement(sub_stmt, target, ranges);
            }
        }
        Statement::While(block) => {
            collect_call_ranges_from_expression(&block.cond, target, ranges);
            for sub_stmt in &block.stmts {
                collect_call_ranges_from_statement(sub_stmt, target, ranges);
            }
        }
        Statement::FunctionCall { comp, args } => {
            if get_function_name(comp) == target {
                if let Some(loc) = comp.parts.first().map(|p| &p.ident.location) {
                    ranges.push(Range {
                        start: Position {
                            line: loc.start_line.saturating_sub(1),
                            character: loc.start_column.saturating_sub(1),
                        },
                        end: Position {
                            line: loc.start_line.saturating_sub(1),
                            character: loc.start_column.saturating_sub(1) + target.len() as u32,
                        },
                    });
                }
            }
            for arg in args {
                collect_call_ranges_from_expression(arg, target, ranges);
            }
        }
        _ => {}
    }
}

/// Collect call ranges from an expression
fn collect_call_ranges_from_expression(expr: &Expression, target: &str, ranges: &mut Vec<Range>) {
    match expr {
        Expression::FunctionCall { comp, args } => {
            if get_function_name(comp) == target {
                if let Some(loc) = comp.parts.first().map(|p| &p.ident.location) {
                    ranges.push(Range {
                        start: Position {
                            line: loc.start_line.saturating_sub(1),
                            character: loc.start_column.saturating_sub(1),
                        },
                        end: Position {
                            line: loc.start_line.saturating_sub(1),
                            character: loc.start_column.saturating_sub(1) + target.len() as u32,
                        },
                    });
                }
            }
            for arg in args {
                collect_call_ranges_from_expression(arg, target, ranges);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            collect_call_ranges_from_expression(lhs, target, ranges);
            collect_call_ranges_from_expression(rhs, target, ranges);
        }
        Expression::Unary { rhs, .. } => {
            collect_call_ranges_from_expression(rhs, target, ranges);
        }
        Expression::Array { elements } => {
            for elem in elements {
                collect_call_ranges_from_expression(elem, target, ranges);
            }
        }
        Expression::If { branches, else_branch } => {
            for (cond, then_expr) in branches {
                collect_call_ranges_from_expression(cond, target, ranges);
                collect_call_ranges_from_expression(then_expr, target, ranges);
            }
            collect_call_ranges_from_expression(else_branch, target, ranges);
        }
        _ => {}
    }
}

/// Collect function calls from an equation
fn collect_function_calls_from_equation(eq: &Equation, calls: &mut HashMap<String, Vec<Range>>) {
    match eq {
        Equation::Simple { lhs, rhs } => {
            collect_function_calls_from_expression(lhs, calls);
            collect_function_calls_from_expression(rhs, calls);
        }
        Equation::For { equations, .. } => {
            for sub_eq in equations {
                collect_function_calls_from_equation(sub_eq, calls);
            }
        }
        Equation::If { cond_blocks, else_block } => {
            for block in cond_blocks {
                collect_function_calls_from_expression(&block.cond, calls);
                for eq in &block.eqs {
                    collect_function_calls_from_equation(eq, calls);
                }
            }
            if let Some(else_eqs) = else_block {
                for eq in else_eqs {
                    collect_function_calls_from_equation(eq, calls);
                }
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                collect_function_calls_from_expression(&block.cond, calls);
                for eq in &block.eqs {
                    collect_function_calls_from_equation(eq, calls);
                }
            }
        }
        Equation::FunctionCall { comp, args } => {
            let name = get_function_name(comp);
            if let Some(loc) = comp.parts.first().map(|p| &p.ident.location) {
                let range = Range {
                    start: Position {
                        line: loc.start_line.saturating_sub(1),
                        character: loc.start_column.saturating_sub(1),
                    },
                    end: Position {
                        line: loc.start_line.saturating_sub(1),
                        character: loc.start_column.saturating_sub(1) + name.len() as u32,
                    },
                };
                calls.entry(name).or_default().push(range);
            }
            for arg in args {
                collect_function_calls_from_expression(arg, calls);
            }
        }
        _ => {}
    }
}

/// Collect function calls from a statement
fn collect_function_calls_from_statement(stmt: &Statement, calls: &mut HashMap<String, Vec<Range>>) {
    match stmt {
        Statement::Assignment { value, .. } => {
            collect_function_calls_from_expression(value, calls);
        }
        Statement::For { equations, .. } => {
            for sub_stmt in equations {
                collect_function_calls_from_statement(sub_stmt, calls);
            }
        }
        Statement::While(block) => {
            collect_function_calls_from_expression(&block.cond, calls);
            for sub_stmt in &block.stmts {
                collect_function_calls_from_statement(sub_stmt, calls);
            }
        }
        Statement::FunctionCall { comp, args } => {
            let name = get_function_name(comp);
            if let Some(loc) = comp.parts.first().map(|p| &p.ident.location) {
                let range = Range {
                    start: Position {
                        line: loc.start_line.saturating_sub(1),
                        character: loc.start_column.saturating_sub(1),
                    },
                    end: Position {
                        line: loc.start_line.saturating_sub(1),
                        character: loc.start_column.saturating_sub(1) + name.len() as u32,
                    },
                };
                calls.entry(name).or_default().push(range);
            }
            for arg in args {
                collect_function_calls_from_expression(arg, calls);
            }
        }
        _ => {}
    }
}

/// Collect function calls from an expression
fn collect_function_calls_from_expression(expr: &Expression, calls: &mut HashMap<String, Vec<Range>>) {
    match expr {
        Expression::FunctionCall { comp, args } => {
            let name = get_function_name(comp);
            if let Some(loc) = comp.parts.first().map(|p| &p.ident.location) {
                let range = Range {
                    start: Position {
                        line: loc.start_line.saturating_sub(1),
                        character: loc.start_column.saturating_sub(1),
                    },
                    end: Position {
                        line: loc.start_line.saturating_sub(1),
                        character: loc.start_column.saturating_sub(1) + name.len() as u32,
                    },
                };
                calls.entry(name).or_default().push(range);
            }
            for arg in args {
                collect_function_calls_from_expression(arg, calls);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            collect_function_calls_from_expression(lhs, calls);
            collect_function_calls_from_expression(rhs, calls);
        }
        Expression::Unary { rhs, .. } => {
            collect_function_calls_from_expression(rhs, calls);
        }
        Expression::Array { elements } => {
            for elem in elements {
                collect_function_calls_from_expression(elem, calls);
            }
        }
        Expression::If { branches, else_branch } => {
            for (cond, then_expr) in branches {
                collect_function_calls_from_expression(cond, calls);
                collect_function_calls_from_expression(then_expr, calls);
            }
            collect_function_calls_from_expression(else_branch, calls);
        }
        _ => {}
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
