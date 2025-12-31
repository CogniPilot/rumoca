//! Symbol collection for diagnostics.
//!
//! Provides helper functions for converting analysis results to LSP diagnostics.
//! Reference checking uses the unified `ir::analysis::reference_checker` module.
//! Type checking is delegated to the shared `ir::analysis::semantic` module.

use lsp_types::{Diagnostic, DiagnosticSeverity};

use crate::ir::analysis::semantic::{TypeCheckResult, TypeErrorSeverity};

use super::helpers::create_diagnostic;

/// Convert type errors from the type checker to LSP diagnostics
pub fn type_errors_to_diagnostics(type_result: &TypeCheckResult) -> Vec<Diagnostic> {
    type_result
        .errors
        .iter()
        .map(|err| {
            let severity = match err.severity {
                TypeErrorSeverity::Warning => DiagnosticSeverity::WARNING,
                TypeErrorSeverity::Error => DiagnosticSeverity::ERROR,
            };
            create_diagnostic(
                err.location.start_line,
                err.location.start_column,
                err.message.clone(),
                severity,
            )
        })
        .collect()
}
