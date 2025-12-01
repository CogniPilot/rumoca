//! Find All References handler for Modelica files.
//!
//! Finds all usages of a symbol (variable, class, function) across the document.

use std::collections::HashMap;

use lsp_types::{Location, Position, Range, ReferenceParams, Uri};

use crate::ir::ast::{ClassDefinition, ComponentReference, Equation, Expression, Statement};

use super::utils::{get_word_at_position, parse_document};

/// Handle find references request
pub fn handle_references(
    documents: &HashMap<Uri, String>,
    params: ReferenceParams,
) -> Option<Vec<Location>> {
    let uri = &params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;
    let include_declaration = params.context.include_declaration;

    let text = documents.get(uri)?;
    let path = uri.path().as_str();

    let word = get_word_at_position(text, position)?;
    let ast = parse_document(text, path)?;

    let mut locations = Vec::new();

    // Find all references in the AST
    for class in ast.class_list.values() {
        collect_references_in_class(class, &word, uri, include_declaration, &mut locations);
    }

    if locations.is_empty() {
        None
    } else {
        Some(locations)
    }
}

/// Collect all references to a symbol within a class
fn collect_references_in_class(
    class: &ClassDefinition,
    name: &str,
    uri: &Uri,
    include_declaration: bool,
    locations: &mut Vec<Location>,
) {
    // Check if this class name matches
    if class.name.text == name && include_declaration {
        locations.push(Location {
            uri: uri.clone(),
            range: token_to_range(&class.name),
        });
    }

    // Check component declarations
    for (comp_name, comp) in &class.components {
        if comp_name == name {
            if include_declaration {
                // The declaration itself
                if let Some(first_token) = comp.type_name.name.first() {
                    // Estimate component name position after type
                    let line = first_token.location.start_line.saturating_sub(1);
                    let type_end = first_token.location.start_column.saturating_sub(1)
                        + first_token.text.len() as u32;
                    locations.push(Location {
                        uri: uri.clone(),
                        range: Range {
                            start: Position {
                                line,
                                character: type_end + 1,
                            },
                            end: Position {
                                line,
                                character: type_end + 1 + comp_name.len() as u32,
                            },
                        },
                    });
                }
            }
        }

        // Check if the type name references our symbol
        for token in &comp.type_name.name {
            if token.text == name {
                locations.push(Location {
                    uri: uri.clone(),
                    range: token_to_range(token),
                });
            }
        }

        // Check start expression (initialization)
        collect_references_in_expression(&comp.start, name, uri, locations);
    }

    // Check equations
    for eq in &class.equations {
        collect_references_in_equation(eq, name, uri, locations);
    }

    // Check initial equations
    for eq in &class.initial_equations {
        collect_references_in_equation(eq, name, uri, locations);
    }

    // Check algorithms
    for algo in &class.algorithms {
        for stmt in algo {
            collect_references_in_statement(stmt, name, uri, locations);
        }
    }

    // Check initial algorithms
    for algo in &class.initial_algorithms {
        for stmt in algo {
            collect_references_in_statement(stmt, name, uri, locations);
        }
    }

    // Check nested classes recursively
    for (nested_name, nested_class) in &class.classes {
        if nested_name == name && include_declaration {
            locations.push(Location {
                uri: uri.clone(),
                range: token_to_range(&nested_class.name),
            });
        }
        collect_references_in_class(nested_class, name, uri, include_declaration, locations);
    }
}

/// Collect references in an equation
fn collect_references_in_equation(
    eq: &Equation,
    name: &str,
    uri: &Uri,
    locations: &mut Vec<Location>,
) {
    match eq {
        Equation::Empty => {}
        Equation::Simple { lhs, rhs } => {
            collect_references_in_expression(lhs, name, uri, locations);
            collect_references_in_expression(rhs, name, uri, locations);
        }
        Equation::Connect { lhs, rhs } => {
            collect_references_in_component_ref(lhs, name, uri, locations);
            collect_references_in_component_ref(rhs, name, uri, locations);
        }
        Equation::For { indices, equations } => {
            // Check for loop indices
            for index in indices {
                if index.ident.text == name {
                    locations.push(Location {
                        uri: uri.clone(),
                        range: token_to_range(&index.ident),
                    });
                }
                collect_references_in_expression(&index.range, name, uri, locations);
            }
            for sub_eq in equations {
                collect_references_in_equation(sub_eq, name, uri, locations);
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                collect_references_in_expression(&block.cond, name, uri, locations);
                for sub_eq in &block.eqs {
                    collect_references_in_equation(sub_eq, name, uri, locations);
                }
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                collect_references_in_expression(&block.cond, name, uri, locations);
                for sub_eq in &block.eqs {
                    collect_references_in_equation(sub_eq, name, uri, locations);
                }
            }
            if let Some(else_eqs) = else_block {
                for sub_eq in else_eqs {
                    collect_references_in_equation(sub_eq, name, uri, locations);
                }
            }
        }
        Equation::FunctionCall { comp, args } => {
            collect_references_in_component_ref(comp, name, uri, locations);
            for arg in args {
                collect_references_in_expression(arg, name, uri, locations);
            }
        }
    }
}

/// Collect references in a statement
fn collect_references_in_statement(
    stmt: &Statement,
    name: &str,
    uri: &Uri,
    locations: &mut Vec<Location>,
) {
    match stmt {
        Statement::Empty => {}
        Statement::Assignment { comp, value } => {
            collect_references_in_component_ref(comp, name, uri, locations);
            collect_references_in_expression(value, name, uri, locations);
        }
        Statement::FunctionCall { comp, args } => {
            collect_references_in_component_ref(comp, name, uri, locations);
            for arg in args {
                collect_references_in_expression(arg, name, uri, locations);
            }
        }
        Statement::For { indices, equations } => {
            for index in indices {
                if index.ident.text == name {
                    locations.push(Location {
                        uri: uri.clone(),
                        range: token_to_range(&index.ident),
                    });
                }
                collect_references_in_expression(&index.range, name, uri, locations);
            }
            for sub_stmt in equations {
                collect_references_in_statement(sub_stmt, name, uri, locations);
            }
        }
        Statement::While(block) => {
            collect_references_in_expression(&block.cond, name, uri, locations);
            for sub_stmt in &block.stmts {
                collect_references_in_statement(sub_stmt, name, uri, locations);
            }
        }
        Statement::Return { .. } | Statement::Break { .. } => {}
    }
}

/// Collect references in an expression
fn collect_references_in_expression(
    expr: &Expression,
    name: &str,
    uri: &Uri,
    locations: &mut Vec<Location>,
) {
    match expr {
        Expression::Empty => {}
        Expression::ComponentReference(comp_ref) => {
            collect_references_in_component_ref(comp_ref, name, uri, locations);
        }
        Expression::Terminal { .. } => {}
        Expression::FunctionCall { comp, args } => {
            collect_references_in_component_ref(comp, name, uri, locations);
            for arg in args {
                collect_references_in_expression(arg, name, uri, locations);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            collect_references_in_expression(lhs, name, uri, locations);
            collect_references_in_expression(rhs, name, uri, locations);
        }
        Expression::Unary { rhs, .. } => {
            collect_references_in_expression(rhs, name, uri, locations);
        }
        Expression::Array { elements } => {
            for element in elements {
                collect_references_in_expression(element, name, uri, locations);
            }
        }
        Expression::Tuple { elements } => {
            for element in elements {
                collect_references_in_expression(element, name, uri, locations);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                collect_references_in_expression(cond, name, uri, locations);
                collect_references_in_expression(then_expr, name, uri, locations);
            }
            collect_references_in_expression(else_branch, name, uri, locations);
        }
        Expression::Range { start, step, end } => {
            collect_references_in_expression(start, name, uri, locations);
            if let Some(step_expr) = step {
                collect_references_in_expression(step_expr, name, uri, locations);
            }
            collect_references_in_expression(end, name, uri, locations);
        }
    }
}

/// Collect references in a component reference
fn collect_references_in_component_ref(
    comp_ref: &ComponentReference,
    name: &str,
    uri: &Uri,
    locations: &mut Vec<Location>,
) {
    for part in &comp_ref.parts {
        if part.ident.text == name {
            locations.push(Location {
                uri: uri.clone(),
                range: token_to_range(&part.ident),
            });
        }
        // Check subscripts
        if let Some(subs) = &part.subs {
            for sub in subs {
                match sub {
                    crate::ir::ast::Subscript::Empty => {}
                    crate::ir::ast::Subscript::Expression(expr) => {
                        collect_references_in_expression(expr, name, uri, locations);
                    }
                    crate::ir::ast::Subscript::Range { .. } => {}
                }
            }
        }
    }
}

/// Convert a token to an LSP range
fn token_to_range(token: &crate::ir::ast::Token) -> Range {
    Range {
        start: Position {
            line: token.location.start_line.saturating_sub(1),
            character: token.location.start_column.saturating_sub(1),
        },
        end: Position {
            line: token.location.start_line.saturating_sub(1),
            character: token.location.start_column.saturating_sub(1) + token.text.len() as u32,
        },
    }
}
