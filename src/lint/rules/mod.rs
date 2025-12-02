//! Individual lint rules for Modelica code.
//!
//! This module contains all lint rules organized by category:
//! - `naming`: Naming convention checks
//! - `references`: Unused/undefined variable detection
//! - `structure`: Class structure, parameters, empty sections
//! - `expressions`: Magic numbers and expression complexity

mod expressions;
mod naming;
mod references;
mod structure;

pub use expressions::{lint_complex_expressions, lint_magic_numbers};
pub use naming::lint_naming_conventions;
pub use references::{lint_undefined_references, lint_unused_variables};
pub use structure::{
    lint_empty_sections, lint_missing_documentation, lint_parameter_defaults,
    lint_redundant_extends, lint_unit_consistency,
};

use super::LintLevel;

/// List of all available lint rules
pub const LINT_RULES: &[(&str, &str, LintLevel)] = &[
    (
        "naming-convention",
        "Check naming conventions (CamelCase for types, camelCase for variables)",
        LintLevel::Note,
    ),
    (
        "missing-documentation",
        "Warn about classes without documentation strings",
        LintLevel::Note,
    ),
    (
        "unused-variable",
        "Detect declared but unused variables",
        LintLevel::Warning,
    ),
    (
        "undefined-reference",
        "Detect references to undefined variables",
        LintLevel::Error,
    ),
    (
        "parameter-no-default",
        "Warn about parameters without default values",
        LintLevel::Help,
    ),
    (
        "empty-section",
        "Detect empty equation or algorithm sections",
        LintLevel::Note,
    ),
    (
        "magic-number",
        "Suggest using named constants instead of magic numbers",
        LintLevel::Help,
    ),
    (
        "complex-expression",
        "Warn about overly complex expressions",
        LintLevel::Note,
    ),
    (
        "inconsistent-units",
        "Check for potential unit inconsistencies",
        LintLevel::Warning,
    ),
    (
        "redundant-extends",
        "Detect redundant or circular extends",
        LintLevel::Warning,
    ),
];
