//! Rust-side unit tests for the pure compile/diagnostics helpers. The full
//! typed-surface behaviour (Model, Result, views) is covered by the Python
//! contract tests in `tests/api_test.py`.

use super::*;

#[test]
fn version_is_non_empty() {
    assert!(!version().is_empty());
}

#[test]
fn diagnostics_report_syntax_error() {
    // Missing `;` after `Real x` is a syntax error.
    let diags = diagnostics_for_source("model M Real x end M;", "input.mo");
    assert!(
        diags.iter().any(|d| d.level == "error"),
        "expected an error diagnostic, got {diags:?}"
    );
}

#[test]
fn diagnostics_clean_source_has_no_syntax_error() {
    let diags = diagnostics_for_source("model M Real x; end M;", "input.mo");
    assert!(
        diags
            .iter()
            .all(|d| d.rule.as_deref() != Some("syntax-error")),
        "valid source should not report a syntax error, got {diags:?}"
    );
}

#[test]
fn builtin_targets_include_sympy() {
    let ids: Vec<String> = targets::list_targets().into_iter().map(|t| t.id).collect();
    assert!(
        ids.iter().any(|id| id == "sympy"),
        "expected a sympy target, got {ids:?}"
    );
}

#[test]
fn solver_listing_has_known_families() {
    let solvers = targets::list_solvers();
    assert!(
        solvers
            .iter()
            .any(|s| s.id == "rk-like" && s.family == "explicit")
    );
    assert!(
        solvers
            .iter()
            .any(|s| s.id == "bdf" && s.family == "implicit")
    );
}
