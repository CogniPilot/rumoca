//! GALEC `.alg` diagnostics pipeline, WASM-safe (lsp-types only — no tower-lsp
//! / tokio), so the same code powers the native server and a future in-browser
//! `.alg` playground.
//!
//! [`compute_diagnostics`] parses the source; a parse error is reported alone
//! (there is no AST to analyse), otherwise the validator's findings are each
//! positioned by resolving their structural location against the parsed AST
//! ([`rumoca_ir_galec::span_of`]). Every diagnostic carries its stable `EG0xx`
//! code and points at the offending source range.

use lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString, Range};

use rumoca_ir_galec::ast::Block;
use rumoca_ir_galec::parse::{GalecParseError, parse};
use rumoca_ir_galec::{GalecError, span_of, validate};

use rumoca_lsp_position::span_to_range;

/// The `source` label attached to every GALEC diagnostic (shown by editors).
const DIAGNOSTIC_SOURCE: &str = "rumoca-galec";

/// Compute LSP diagnostics for one GALEC `.alg` document. A syntax error is
/// reported on its own (no AST to analyse); an accepted document reports its
/// validator findings, or none when it is well-formed.
#[must_use]
pub fn compute_diagnostics(source: &str, file_name: &str) -> Vec<Diagnostic> {
    match parse(source, file_name) {
        Err(error) => vec![parse_error_to_diagnostic(&error, source)],
        Ok(block) => match validate(&block) {
            Ok(()) => Vec::new(),
            Err(errors) => errors
                .iter()
                .map(|error| validator_error_to_diagnostic(error, &block, source))
                .collect(),
        },
    }
}

/// A parse error, positioned by its byte offsets when parol supplied them.
fn parse_error_to_diagnostic(error: &GalecParseError, source: &str) -> Diagnostic {
    let range = match error.span() {
        Some((start, end)) => span_to_range(source, start, end),
        None => first_line_range(source),
    };
    diagnostic(range, error.code(), error.to_string())
}

/// A validator finding, positioned by resolving its structural location to a
/// source span over the parsed AST.
fn validator_error_to_diagnostic(error: &GalecError, block: &Block, source: &str) -> Diagnostic {
    let range = match span_of(block, error.location()) {
        Some(span) => span_to_range(source, span.start.0, span.end.0),
        None => first_line_range(source),
    };
    diagnostic(range, error.code(), error.to_string())
}

/// Assemble an error-severity diagnostic with a stable string code.
fn diagnostic(range: Range, code: &str, message: String) -> Diagnostic {
    Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        code: Some(NumberOrString::String(code.to_string())),
        source: Some(DIAGNOSTIC_SOURCE.to_string()),
        message,
        ..Default::default()
    }
}

/// Fallback range for a diagnostic whose span is unknown: the first source line
/// (never an empty or out-of-bounds range).
fn first_line_range(source: &str) -> Range {
    let end = source.find('\n').unwrap_or(source.len());
    span_to_range(source, 0, end)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn well_formed_document_has_no_diagnostics() {
        // Print a minimal valid block, then diagnose its text.
        let block = rumoca_ir_galec::ast::Block::new(rumoca_ir_galec::ast::Name::ident("Ok"));
        let text = rumoca_ir_galec::print_block(&block).expect("prints");
        assert!(
            compute_diagnostics(&text, "ok.alg").is_empty(),
            "a well-formed block should produce no diagnostics"
        );
    }

    #[test]
    fn syntax_error_is_reported_and_positioned() {
        let bad = "block Bad\nprotected\npublic\n\
                   method Startup\nalgorithm\nend Startup;\n\
                   method Recalibrate\nalgorithm\nend Recalibrate;\n\
                   method DoStep\nalgorithm\n1 := 2;\nend DoStep;\n\
                   end Bad;\n";
        let diags = compute_diagnostics(bad, "bad.alg");
        assert_eq!(diags.len(), 1, "one syntax error expected");
        let diag = &diags[0];
        assert_eq!(diag.severity, Some(DiagnosticSeverity::ERROR));
        assert!(matches!(&diag.code, Some(NumberOrString::String(c)) if c.starts_with("EG05")));
        // Positioned at the offending body, not pinned at the document start.
        assert!(diag.range.start.line > 0 || diag.range.start.character > 0);
    }

    #[test]
    fn terminator_mismatch_is_positioned_at_the_footer() {
        // `block Foo … end Bar;` — the block terminator name mismatches. The
        // EG051 diagnostic must point at the offending `end Bar`, not line 1.
        let bad = "block Foo\nprotected\npublic\n\
                   method Startup\nalgorithm\nend Startup;\n\
                   method Recalibrate\nalgorithm\nend Recalibrate;\n\
                   method DoStep\nalgorithm\nend DoStep;\n\
                   end Bar;\n";
        let diags = compute_diagnostics(bad, "mismatch.alg");
        let diag = diags
            .iter()
            .find(|d| matches!(&d.code, Some(NumberOrString::String(c)) if c == "EG051"))
            .expect("EG051 terminator-mismatch diagnostic");
        let line = bad.lines().nth(diag.range.start.line as usize).unwrap();
        let start = diag.range.start.character as usize;
        let end = diag.range.end.character as usize;
        assert_eq!(
            &line[start..end],
            "Bar",
            "positioned at the `end Bar` footer"
        );
    }

    #[test]
    fn validator_error_is_reported_and_positioned() {
        // Two interface inputs named `u` -> EG012 duplicate name.
        use rumoca_ir_galec::ast::{
            Block, InterfaceKind, InterfaceVariable, Name, ScalarType, VariableDeclaration,
        };
        let input = |name: &str| InterfaceVariable {
            kind: InterfaceKind::Input,
            decl: VariableDeclaration::scalar(ScalarType::Real, Name::ident(name)),
            start: None,
        };
        let mut block = Block::new(Name::ident("Dup"));
        block.interface.push(input("u"));
        block.interface.push(input("u"));
        let text = rumoca_ir_galec::print_block(&block).expect("prints");

        let diags = compute_diagnostics(&text, "dup.alg");
        let dup = diags
            .iter()
            .find(|d| matches!(&d.code, Some(NumberOrString::String(c)) if c == "EG012"))
            .expect("EG012 duplicate-name diagnostic");
        // The range slices the declaration `u`.
        let line = text.lines().nth(dup.range.start.line as usize).unwrap();
        let start = dup.range.start.character as usize;
        let end = dup.range.end.character as usize;
        assert_eq!(&line[start..end], "u");
    }
}
