//! Helper functions for diagnostics.

use lsp_types::{Diagnostic, DiagnosticSeverity, Position, Range};

/// Create a diagnostic at a specific location
pub fn create_diagnostic(
    line: u32,
    col: u32,
    message: String,
    severity: DiagnosticSeverity,
) -> Diagnostic {
    Diagnostic {
        range: Range {
            start: Position {
                line: line.saturating_sub(1),
                character: col.saturating_sub(1),
            },
            end: Position {
                line: line.saturating_sub(1),
                character: col.saturating_sub(1) + 20,
            },
        },
        severity: Some(severity),
        source: Some("rumoca".to_string()),
        message,
        ..Default::default()
    }
}
