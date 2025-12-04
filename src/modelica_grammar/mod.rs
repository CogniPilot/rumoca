//! This module provides implementations for converting the automatic Parol AST
//! (Abstract Syntax Tree) into the internal AST representation (`crate::ir::ast`).
//!
//! The module includes various `TryFrom` implementations for converting between
//! the `modelica_grammar_trait` types and the internal `ir::ast` types.

mod components;
mod definitions;
mod equations;
mod expressions;
pub mod generated;
mod helpers;
mod references;
mod sections;

use crate::ir;
use generated::modelica_grammar_trait;
use parol_runtime::{Result, Token};
use std::fmt::{Display, Error, Formatter};

// Re-export types used by modelica_grammar_trait (generated code references these)
pub use components::{ComponentList, TokenList};
pub use definitions::{Composition, ElementList};
pub use expressions::{ArraySubscripts, ExpressionList, ModificationArg};
pub use sections::{AlgorithmSection, EquationSection};

/// A parsed comment with its location information
#[derive(Debug, Clone, Default)]
pub struct ParsedComment {
    /// The comment text (including // or /* */)
    pub text: String,
    /// Line number (1-based)
    pub line: u32,
    /// Column number (1-based)
    pub column: u32,
    /// Whether this is a line comment (//) or block comment (/* */)
    pub is_line_comment: bool,
}

#[derive(Debug, Default)]
pub struct ModelicaGrammar<'t> {
    pub modelica: Option<ir::ast::StoredDefinition>,
    /// Comments collected during parsing, in order of appearance
    pub comments: Vec<ParsedComment>,
    _phantom: std::marker::PhantomData<&'t str>,
}

impl ModelicaGrammar<'_> {
    pub fn new() -> Self {
        ModelicaGrammar::default()
    }
}

impl Display for modelica_grammar_trait::StoredDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::result::Result<(), Error> {
        write!(f, "{:?}", self)
    }
}

impl Display for ModelicaGrammar<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::result::Result<(), Error> {
        match &self.modelica {
            Some(modelica) => writeln!(f, "{:#?}", modelica),
            None => write!(f, "No parse result"),
        }
    }
}

impl<'t> modelica_grammar_trait::ModelicaGrammarTrait for ModelicaGrammar<'t> {
    fn stored_definition(&mut self, arg: &modelica_grammar_trait::StoredDefinition) -> Result<()> {
        self.modelica = Some(arg.try_into()?);
        Ok(())
    }

    /// Collect comments during parsing for later use (e.g., in formatter)
    fn on_comment(&mut self, token: Token<'_>) {
        let text = token.text().to_string();
        let is_line_comment = text.starts_with("//");

        self.comments.push(ParsedComment {
            text,
            line: token.location.start_line,
            column: token.location.start_column,
            is_line_comment,
        });
    }
}
