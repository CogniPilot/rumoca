//! Semantic analysis for Modelica equations and statements.
//!
//! This module provides semantic checking capabilities that can be used by both
//! the compiler (for validation) and the LSP (for diagnostics).
//!
//! It includes type checking, when/reinit restrictions, connect validation,
//! and other semantic constraints from the Modelica Language Specification.

mod algorithms;
mod classes;
mod components;
mod connectors;
mod context;
mod equations;
mod expressions;
mod member_access;
mod subscripts;
mod types;

use crate::ir::ast::Location;

use super::type_inference::SymbolType;

// Re-export public API - Equations (MLS §8)
pub use equations::{
    check_duplicate_reinit, check_edge_argument_type, check_equation,
    check_equation_loop_variable_assignments, check_equation_with_classes, check_equations,
    check_equations_with_classes, check_reinit_context, check_reinit_variable_type,
    check_statement, check_statement_with_classes, check_statements, check_statements_with_classes,
    check_when_condition_type, check_when_restrictions,
};

// Re-export public API - Connectors (MLS §9)
pub use connectors::{
    check_cardinality_arguments, check_cardinality_context, check_connect_types,
    check_connector_no_parameter_constant, check_expandable_no_flow, check_flow_compatibility,
    check_self_connections, check_stream_only_in_connector, check_stream_requires_flow,
};

// Re-export public API - Algorithms (MLS §11)
pub use algorithms::{
    check_algorithm_assignments_with_types, check_algorithm_condition_types,
    check_assert_arguments, check_loop_variable_assignments,
};

// Re-export public API - Classes (MLS §12, §14, §16, §17)
pub use classes::{
    check_clock_restrictions, check_function_component_classes, check_function_forbidden_operators,
    check_function_no_inner_outer, check_function_public_components,
    check_function_single_algorithm, check_operator_record_cannot_be_extended,
    check_operator_record_no_extends, check_operator_record_no_partial, check_single_initial_state,
    check_state_is_block_or_model, check_transition_priority_unique,
};

// Re-export public API - Components (MLS §4, §7)
pub use components::{
    check_builtin_attribute_modifiers, check_component_bindings, check_constant_bindings,
    check_variability_dependencies,
};

// Re-export public API - Expressions
pub use expressions::validate_expression;
pub use member_access::check_class_member_access;

// Re-export public API - Subscripts and bounds
pub use subscripts::{
    check_array_bounds, check_array_dimension_mismatch, check_array_index_types,
    check_inconsistent_nested_arrays, check_scalar_subscripts,
};

// Re-export public API - Context validation
pub use context::{check_break_return_context, check_start_modification_dimensions};

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
