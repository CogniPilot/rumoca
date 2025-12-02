//! Find All References handler for Modelica files.
//!
//! Finds all usages of a symbol (variable, class, function) across the document.

use std::collections::HashMap;

use lsp_types::{Location, ReferenceParams, Uri};

use crate::ir::ast::{ClassDefinition, Component, ComponentReference, Token};
use crate::ir::visitor::{Visitable, Visitor};

use crate::lsp::utils::{get_word_at_position, parse_document, token_to_range};

/// Visitor that finds all references to a specific symbol name
struct ReferenceFinder<'a> {
    /// The symbol name to find
    name: &'a str,
    /// The URI for creating Location objects
    uri: &'a Uri,
    /// Whether to include declarations
    include_declaration: bool,
    /// Collected locations
    locations: Vec<Location>,
}

impl<'a> ReferenceFinder<'a> {
    fn new(name: &'a str, uri: &'a Uri, include_declaration: bool) -> Self {
        Self {
            name,
            uri,
            include_declaration,
            locations: Vec::new(),
        }
    }

    fn add_location_from_token(&mut self, token: &Token) {
        self.locations.push(Location {
            uri: self.uri.clone(),
            range: token_to_range(token),
        });
    }
}

impl Visitor for ReferenceFinder<'_> {
    fn enter_class_definition(&mut self, node: &ClassDefinition) {
        // Check class name (declaration)
        if node.name.text == self.name && self.include_declaration {
            self.add_location_from_token(&node.name);
        }

        // Check nested class names (declarations)
        for (nested_name, nested_class) in &node.classes {
            if nested_name == self.name && self.include_declaration {
                self.add_location_from_token(&nested_class.name);
            }
        }

        // Check for loop indices in equations
        for eq in &node.equations {
            self.check_equation_indices(eq);
        }
        for eq in &node.initial_equations {
            self.check_equation_indices(eq);
        }

        // Check for loop indices in statements
        for algo in &node.algorithms {
            for stmt in algo {
                self.check_statement_indices(stmt);
            }
        }
        for algo in &node.initial_algorithms {
            for stmt in algo {
                self.check_statement_indices(stmt);
            }
        }
    }

    fn enter_component(&mut self, node: &Component) {
        // Check component name (declaration)
        if node.name_token.text == self.name && self.include_declaration {
            self.add_location_from_token(&node.name_token);
        }

        // Check type name references
        for token in &node.type_name.name {
            if token.text == self.name {
                self.add_location_from_token(token);
            }
        }
    }

    fn enter_component_reference(&mut self, node: &ComponentReference) {
        for part in &node.parts {
            if part.ident.text == self.name {
                self.add_location_from_token(&part.ident);
            }
        }
    }
}

impl ReferenceFinder<'_> {
    /// Check for loop indices in an equation (special handling needed since indices
    /// are not visited by the standard AstVisitor)
    fn check_equation_indices(&mut self, eq: &crate::ir::ast::Equation) {
        match eq {
            crate::ir::ast::Equation::For { indices, equations } => {
                for index in indices {
                    if index.ident.text == self.name {
                        self.add_location_from_token(&index.ident);
                    }
                }
                for sub_eq in equations {
                    self.check_equation_indices(sub_eq);
                }
            }
            crate::ir::ast::Equation::When(blocks) => {
                for block in blocks {
                    for sub_eq in &block.eqs {
                        self.check_equation_indices(sub_eq);
                    }
                }
            }
            crate::ir::ast::Equation::If {
                cond_blocks,
                else_block,
            } => {
                for block in cond_blocks {
                    for sub_eq in &block.eqs {
                        self.check_equation_indices(sub_eq);
                    }
                }
                if let Some(else_eqs) = else_block {
                    for sub_eq in else_eqs {
                        self.check_equation_indices(sub_eq);
                    }
                }
            }
            _ => {}
        }
    }

    /// Check for loop indices in a statement
    fn check_statement_indices(&mut self, stmt: &crate::ir::ast::Statement) {
        match stmt {
            crate::ir::ast::Statement::For { indices, equations } => {
                for index in indices {
                    if index.ident.text == self.name {
                        self.add_location_from_token(&index.ident);
                    }
                }
                for sub_stmt in equations {
                    self.check_statement_indices(sub_stmt);
                }
            }
            crate::ir::ast::Statement::While(block) => {
                for sub_stmt in &block.stmts {
                    self.check_statement_indices(sub_stmt);
                }
            }
            crate::ir::ast::Statement::If {
                cond_blocks,
                else_block,
            } => {
                for block in cond_blocks {
                    for sub_stmt in &block.stmts {
                        self.check_statement_indices(sub_stmt);
                    }
                }
                if let Some(else_stmts) = else_block {
                    for sub_stmt in else_stmts {
                        self.check_statement_indices(sub_stmt);
                    }
                }
            }
            crate::ir::ast::Statement::When(blocks) => {
                for block in blocks {
                    for sub_stmt in &block.stmts {
                        self.check_statement_indices(sub_stmt);
                    }
                }
            }
            _ => {}
        }
    }
}

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

    let mut finder = ReferenceFinder::new(&word, uri, include_declaration);
    ast.accept(&mut finder);

    if finder.locations.is_empty() {
        None
    } else {
        Some(finder.locations)
    }
}
