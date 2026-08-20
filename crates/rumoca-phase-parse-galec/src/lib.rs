//! Parse phase for GALEC Algorithm Code (`.alg`) sources.
//!
//! Parsing is separate from the checked language representation. This crate
//! owns the grammar, generated parser, parser actions, and positioned syntax
//! diagnostics; `rumoca-ir-galec` owns the resulting language data and checked
//! constructors.

mod parse;

use rumoca_core::Span;
use rumoca_ir_galec::package::{CheckedAlgorithmBlock, PackageError};

// The generated parser addresses these modules at crate root. They stay
// private so generated CST and parser state cannot escape the phase boundary.
mod grammar {
    pub(crate) use crate::parse::GalecGrammar;
}
use parse::generated::galec_grammar_trait as grammar_trait;

pub use parse::GalecSyntaxError;

/// Failure to parse and close one checked GALEC block.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum GalecParseError {
    #[error(transparent)]
    Syntax(#[from] GalecSyntaxError),
    #[error(transparent)]
    Invalid(#[from] PackageError),
}

/// Opaque syntax document retained only for editor diagnostics/navigation.
///
/// The unvalidated block never escapes this phase. Production compilation uses
/// [`parse`], which returns a checked block.
#[derive(Debug)]
pub struct GalecDocument {
    block: rumoca_ir_galec::Block,
}

/// One positioned semantic diagnostic for an invalid editor document.
#[derive(Debug, Clone, PartialEq)]
pub struct DocumentDiagnostic {
    code: &'static str,
    message: String,
    span: Option<Span>,
}

/// Resolved source occurrence returned by an opaque editor document.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DocumentSymbol {
    reference_span: Span,
    definition_span: Option<Span>,
    hover: String,
}

impl DocumentSymbol {
    #[must_use]
    pub const fn reference_span(&self) -> Span {
        self.reference_span
    }

    #[must_use]
    pub const fn definition_span(&self) -> Option<Span> {
        self.definition_span
    }

    #[must_use]
    pub fn hover(&self) -> &str {
        &self.hover
    }
}

impl DocumentDiagnostic {
    #[must_use]
    pub const fn code(&self) -> &'static str {
        self.code
    }

    #[must_use]
    pub fn message(&self) -> &str {
        &self.message
    }

    #[must_use]
    pub const fn span(&self) -> Option<Span> {
        self.span
    }
}

impl GalecDocument {
    /// Collect semantic diagnostics without exposing the unchecked block.
    #[must_use]
    pub fn diagnostics(&self) -> Vec<DocumentDiagnostic> {
        match rumoca_ir_galec::validate(&self.block) {
            Ok(()) => Vec::new(),
            Err(errors) => errors
                .iter()
                .map(|error| DocumentDiagnostic {
                    code: error.code(),
                    message: error.to_string(),
                    span: rumoca_ir_galec::span_of(&self.block, error.location()),
                })
                .collect(),
        }
    }

    /// Resolve the source occurrence at a byte offset for editor navigation.
    #[must_use]
    pub fn symbol_at(&self, offset: usize) -> Option<DocumentSymbol> {
        let symbol = rumoca_ir_galec::symbol_at(&self.block, offset)?;
        Some(DocumentSymbol {
            reference_span: symbol.reference_span,
            definition_span: symbol.definition_span,
            hover: symbol.hover,
        })
    }

    /// Close this syntactic document through the checked IR constructor.
    pub fn into_checked(self) -> Result<CheckedAlgorithmBlock, PackageError> {
        CheckedAlgorithmBlock::construct(self.block)
    }
}

/// Parse GALEC source and close it as a checked Algorithm Code block.
pub fn parse(source: &str, file_name: &str) -> Result<CheckedAlgorithmBlock, GalecParseError> {
    parse_document(source, file_name)?
        .into_checked()
        .map_err(GalecParseError::from)
}

/// Parse an editor document while retaining invalid syntax privately in phase.
pub fn parse_document(source: &str, file_name: &str) -> Result<GalecDocument, GalecSyntaxError> {
    Ok(GalecDocument {
        block: parse::parse_block(source, file_name)?,
    })
}

#[cfg(test)]
mod roundtrip_tests;
