//! Shared AST walker for finding name occurrences.

use rumoca_session::parsing::ast;
use rumoca_session::parsing::ir_core as rumoca_ir_core;
use std::ops::ControlFlow::{self, Continue};

use lsp_types::Range;

use crate::helpers::token_to_range;

type ClassDef = ast::ClassDef;
type Component = ast::Component;
type ComponentReference = ast::ComponentReference;
type Expression = ast::Expression;
type StoredDefinition = ast::StoredDefinition;
type Token = rumoca_ir_core::Token;

/// The kind of occurrence found.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OccurrenceKind {
    Declaration,
    TypeReference,
    ComponentReference,
    EndName,
    ClassName,
}

/// A single occurrence of a name in the source.
#[derive(Debug, Clone)]
pub struct TokenOccurrence {
    pub range: Range,
    pub kind: OccurrenceKind,
}

/// Find all occurrences of a name in the AST.
pub fn find_all_occurrences(
    ast: &StoredDefinition,
    name: &str,
    include_decls: bool,
) -> Vec<TokenOccurrence> {
    let mut finder = OccurrenceFinder {
        name: name.to_string(),
        include_decls,
        occurrences: Vec::new(),
    };
    let _ = ast::visitor::Visitor::visit_stored_definition(&mut finder, ast);
    finder.occurrences
}

struct OccurrenceFinder {
    name: String,
    include_decls: bool,
    occurrences: Vec<TokenOccurrence>,
}

impl OccurrenceFinder {
    fn check_token(&mut self, token: &Token, kind: OccurrenceKind) {
        if *token.text == *self.name {
            if !self.include_decls && kind == OccurrenceKind::Declaration {
                return;
            }
            self.occurrences.push(TokenOccurrence {
                range: token_to_range(token),
                kind,
            });
        }
    }
}

impl ast::visitor::Visitor for OccurrenceFinder {
    fn visit_class_def(&mut self, class: &ClassDef) -> ControlFlow<()> {
        // Class name declaration
        self.check_token(&class.name, OccurrenceKind::ClassName);

        // End name token
        if let Some(end_token) = &class.end_name_token {
            self.check_token(end_token, OccurrenceKind::EndName);
        }

        // Visit children
        for ext in &class.extends {
            self.visit_extend(ext)?;
        }
        for (_, nested) in &class.classes {
            self.visit_class_def(nested)?;
        }
        for (_, comp) in &class.components {
            self.visit_component(comp)?;
        }
        self.visit_each(&class.equations, Self::visit_equation)?;
        self.visit_each(&class.initial_equations, Self::visit_equation)?;
        for section in &class.algorithms {
            self.visit_each(section, Self::visit_statement)?;
        }
        for section in &class.initial_algorithms {
            self.visit_each(section, Self::visit_statement)?;
        }
        Continue(())
    }

    fn visit_component(&mut self, comp: &Component) -> ControlFlow<()> {
        // Component name declaration
        self.check_token(&comp.name_token, OccurrenceKind::Declaration);

        // Type reference
        for token in &comp.type_name.name {
            self.check_token(token, OccurrenceKind::TypeReference);
        }

        // Visit children
        if !matches!(comp.start, Expression::Empty) {
            self.visit_expression(&comp.start)?;
        }
        for (_, mod_expr) in &comp.modifications {
            self.visit_expression(mod_expr)?;
        }
        if let Some(cond) = &comp.condition {
            self.visit_expression(cond)?;
        }
        self.visit_each(&comp.annotation, Self::visit_expression)
    }

    fn visit_component_reference(&mut self, cr: &ComponentReference) -> ControlFlow<()> {
        for part in &cr.parts {
            self.check_token(&part.ident, OccurrenceKind::ComponentReference);
            if let Some(subs) = &part.subs {
                self.visit_each(subs, Self::visit_subscript)?;
            }
        }
        Continue(())
    }

    fn visit_expr_function_call(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
    ) -> ControlFlow<()> {
        for part in &comp.parts {
            self.check_token(&part.ident, OccurrenceKind::ComponentReference);
        }
        self.visit_each(args, Self::visit_expression)
    }
}
