//! Type checker for Modelica equations and statements.
//!
//! This module provides type checking capabilities that can be used by both
//! the compiler (for validation) and the LSP (for diagnostics).
//!
//! The type checker builds on the type inference module to detect type mismatches
//! in equations and statements.

mod algorithms;
mod assertions;
mod attributes;
mod bindings;
mod cardinality;
mod equations;
mod member_access;
mod types;
mod validation;

use crate::ir::ast::Location;

use super::type_inference::SymbolType;

// Re-export public API
pub use algorithms::check_algorithm_assignments_with_types;
pub use assertions::check_assert_arguments;
pub use attributes::check_builtin_attribute_modifiers;
pub use bindings::check_component_bindings;
pub use cardinality::{check_cardinality_arguments, check_cardinality_context};
pub use equations::{
    check_equation, check_equation_with_classes, check_equations, check_equations_with_classes,
    check_statement, check_statement_with_classes, check_statements, check_statements_with_classes,
};
pub use member_access::check_class_member_access;
pub use validation::{
    check_array_bounds, check_break_return_context, check_scalar_subscripts,
    check_start_modification_dimensions,
};

/// Severity of a type error
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeErrorSeverity {
    /// A warning that may indicate a problem but doesn't prevent compilation
    Warning,
    /// An error that indicates a definite type mismatch
    Error,
}

/// A type error detected during type checking
#[derive(Debug, Clone)]
pub struct TypeError {
    /// Location in the source code
    pub location: Location,
    /// The expected type (or the LHS type in an equation)
    pub expected: SymbolType,
    /// The actual type found (or the RHS type in an equation)
    pub actual: SymbolType,
    /// Human-readable error message
    pub message: String,
    /// Severity of the error
    pub severity: TypeErrorSeverity,
}

impl TypeError {
    /// Create a new type error
    pub fn new(
        location: Location,
        expected: SymbolType,
        actual: SymbolType,
        message: String,
        severity: TypeErrorSeverity,
    ) -> Self {
        Self {
            location,
            expected,
            actual,
            message,
            severity,
        }
    }

    /// Create a type mismatch warning
    pub fn mismatch(location: Location, lhs: SymbolType, rhs: SymbolType) -> Self {
        // Format message first using Display, then move values to avoid cloning
        let message = format!(
            "Type mismatch in equation: {} is not compatible with {}",
            lhs, rhs
        );
        Self {
            location,
            expected: lhs,
            actual: rhs,
            message,
            severity: TypeErrorSeverity::Warning,
        }
    }

    /// Create a Boolean/numeric mixing error
    pub fn boolean_numeric_mix(location: Location, lhs: SymbolType, rhs: SymbolType) -> Self {
        Self {
            location,
            expected: lhs,
            actual: rhs,
            message: "Cannot mix Boolean and numeric types in equation".to_string(),
            severity: TypeErrorSeverity::Error,
        }
    }
}

/// Result of type checking a class or set of equations
#[derive(Debug, Default)]
pub struct TypeCheckResult {
    /// List of type errors found
    pub errors: Vec<TypeError>,
}

impl TypeCheckResult {
    /// Create a new empty result
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    /// Check if there are any errors (not just warnings)
    pub fn has_errors(&self) -> bool {
        self.errors
            .iter()
            .any(|e| e.severity == TypeErrorSeverity::Error)
    }

    /// Check if there are any issues (errors or warnings)
    pub fn has_issues(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Add an error
    pub fn add_error(&mut self, error: TypeError) {
        self.errors.push(error);
    }

    /// Merge another result into this one
    pub fn merge(&mut self, other: TypeCheckResult) {
        self.errors.extend(other.errors);
    }
}
